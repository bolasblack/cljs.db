(ns mysql.core)

(defmacro with-transaction
  "Execute mysql query in transaction

  Support multiple expr which return `clojure.core.async/Channel`
  pipe `js/Error`, `cats.monad.either/Either`, `[js/Error, <whatever>]`

  The exection will be terminated when receive first error

  Example:

      (with-transaction
       (go (js/Error. \"some error\"))
       (go [(js/Error. \"some error\")])
       (go (cats.monad.either/left (js/Error. \"some error\")))"
  [& body]
  (let [conn `conn#
        err `err#
        chan `chan#
        res `res#
        pick-err `(cond (instance? js/Error ~res)
                        ~res

                        ~@(if (find-ns 'cats.monad.either)
                            (list
                             `(cats.monad.either/either? ~res)
                             `(if (cats.monad.either/left? ~res)
                                (deref ~res)
                                nil))
                            [])

                        (and (vector? ~res)
                             (some? (first ~res)))
                        (first ~res)

                        :else
                        nil)
        wrapped-body ((fn wrap-first-expr [body]
                        (let [[expr & next-body] (or body [])]
                          (if expr
                            `(let [~res (cljs.core.async/<! ~expr)
                                   ~err ~pick-err]
                               (if ~err
                                 [~err nil]
                                 ~(wrap-first-expr next-body)))
                            `[])))
                      body)
        error-handler-expr `(do (.rollback ~conn #(throw ~err))
                                (cljs.core.async/>! ~chan [~err])
                                (cljs.core.async/close! ~chan))]
    `(let [~conn (checker.db/mysql-conn)
           ~chan (cljs.core.async/chan)]
       (.beginTransaction
        ~conn
        (fn [~err]
          (cljs.core.async.macros/go
            (if ~err
              ~error-handler-expr
              (let [[~err] ~wrapped-body]
                (if ~err
                  ~error-handler-expr
                  (.commit
                   ~conn
                   (fn [~err]
                     (cljs.core.async.macros/go
                       (if ~err
                         ~error-handler-expr
                         (do (cljs.core.async/>! ~chan [])
                             (cljs.core.async/close! ~chan))))))))))))
       ~chan)))
