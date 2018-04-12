(ns mysql.core
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [mysql.core])
  (:require ["mysql2" :as m]
            ["lodash.isobjectlike" :as is-object-like]
            [cljs.core.async :as async]
            [utils.core :as uc :include-macros true]
            [utils.async :as ua :include-macros true]))

(defn- normalize->clj [a & opts]
  (-> (js/JSON.stringify a)
      js/JSON.parse
      (#(apply js->clj % opts))))

(defn with-transaction*
  "

  Example:

  ```clojurescript
  (go (<! (with-transaction* connection
            (fn []
              (go (let [posts (<! (exec! \"SELECT * FROM posts\"))]
                    (prn \"posts\" posts)))))))

  (go (<! (with-transaction* {:conn connection :policy :error}
            (fn []
              (go (let [posts (<! (exec! \"SELECT * FROM posts\"))]
                    (prn \"posts\" posts)))))))
  ```"
  [opts cb]
  (ua/go-let [{:keys [conn policy]}
              (if (map? opts) opts {:conn opts})

              transaction-statue
              (volatile! :idle)

              res
              (ua/<! (ua/go-try
                      (ua/<? (ua/denodify.. conn -beginTransaction))
                      (vreset! transaction-statue :started)
                      (ua/<<? (cb) :policy policy)
                      (ua/<? (ua/denodify.. conn -commit))
                      (vreset! transaction-statue :finished)))]
    (when (uc/error? res)
      (when (= :started @transaction-statue)
        (ua/<! (ua/denodify.. conn -rollback)))
      (if (ua/packed-error? res)
        (ua/unpack-error res :policy policy)
        res))))

(defn conn
  "Create mysql connection
  Support both url string or hash:

  Example:

  ```clojurescript
  (conn \"mysql://user:pass@host/db?debug=true&charset=BIG5_CHINESE_CI&timezone=-0700\")

  (conn :user \"root\" :passworld \"\")
  ```

  Visit https://github.com/mysqljs/mysql#connection-options for more connection options description"
  [& config]
  (if (string? (first config))
    (m/createConnection (first config))
    (m/createConnection (clj->js (apply hash-map config)))))

(defn close! [conn]
  (.close conn))

(defn end! [conn]
  (let [chan (async/chan)]
    (.end conn (fn [err]
                 (if err
                   (async/put! chan err #(async/close! chan))
                   (async/put! chan :ok #(async/close! chan)))))
    chan))

(defn exec! [conn query & [args]]
  (let [chan (async/chan)]
    (.execute conn (clj->js query) (clj->js (or args []))
              (fn [err _rows _fields]
                (if err
                  (async/put! chan err #(async/close! chan))
                  (let [rows (normalize->clj _rows :keywordize-keys true)
                        fields (normalize->clj _fields :keywordize-keys true)]
                    (async/put! chan {:rows rows :fields fields} #(async/close! chan))))))
    chan))
