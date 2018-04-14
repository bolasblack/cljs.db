(ns db.mysql-test
  (:require
   [pjstadig.humane-test-output]
   [cljs.test :as ct :refer [deftest testing is]]
   [utils.async :as ua]
   ["mysql2" :as mysql]
   [db.mysql :as m]
   ["lodash.isequal" :as js-equal]))

(deftest conn
  (with-redefs [mysql/createConnection (fn [& args] args)]
    (is (= '("conn string") (m/conn "conn string")))
    (is (= [{"conn-param1" 1 "conn-param2" 2}]
           (js->clj (m/conn :conn-param1 1 :conn-param2 2))))))

(deftest close!
  (let [fake-conn-called (volatile! false)
        fake-conn #js {:close #(vreset! fake-conn-called true)}]
    (m/close! fake-conn)
    (is @fake-conn-called)))

(deftest end!
  (ct/async
   done
   (ua/go-let [fake-error (js/Error.)
               fake-conn-call-time (volatile! 0)
               fake-conn1 (js-obj "end" #(do (vswap! fake-conn-call-time inc)
                                             (%)))
               resp1 (ua/<! (m/end! fake-conn1))
               fake-conn2 (js-obj "end" #(do (vswap! fake-conn-call-time inc)
                                             (% fake-error)))
               resp2 (ua/<! (m/end! fake-conn2))]
     (is (= 2 @fake-conn-call-time))
     (is (= :ok resp1))
     (is (= fake-error resp2))
     (done))))

(deftest exec!
  (ct/async
   done
   (ua/go-let [fake-error (js/Error.)
               fake-conn-args (volatile! [])
               fake-conn1 (js-obj "execute" (fn [& [_ _ callback :as args]]
                                              (vswap! fake-conn-args #(conj % args))
                                              (callback fake-error)))
               resp1 (ua/<! (m/exec! fake-conn1 "sql string" ["sql args1" :sql-args2]))
               resp2 (ua/<! (m/exec! fake-conn1 "sql string"))
               fake-conn2 (js-obj "execute" (fn [& [_ _ callback :as args]]
                                              (vswap! fake-conn-args #(conj % args))
                                              (callback nil #js [#js {:id 1} #js {:id 2}] #js [#js {:fake-fields true}])))
               resp3 (ua/<! (m/exec! fake-conn2 {:sql "sql string" :timeout 5000} ["sql args1" :sql-args2]))]
     (is (= 3 (count @fake-conn-args)))

     (let [args (first @fake-conn-args)]
       (is (= "sql string" (first args)))
       (is (js-equal #js ["sql args1" "sql-args2"] (second args))))
     (is (= fake-error resp1))

     (let [args (nth @fake-conn-args 1)]
       (is (= "sql string" (first args)))
       (is (js-equal #js [] (second args))))
     (is (= fake-error resp2))

     (let [args (nth @fake-conn-args 2)]
       (is (js-equal #js {:sql "sql string" :timeout 5000} (first args)))
       (is (js-equal #js ["sql args1" "sql-args2"] (second args))))
     (is (= [{:id 1} {:id 2}] (:rows resp3)))
     (is (= 1 (count (:fields resp3))))
     (is (js-equal #js {:fake-fields true} (first (:fields resp3))))
     (done))))

(deftest with-transaction*
  (ct/async
   done
   (ua/go-let [method-args {:beginTransaction []
                            :commit []
                            :rollback []
                            :callback []
                            :execute []}
               next-method-cb-args (volatile! method-args)
               catched-method-args (volatile! method-args)
               catched-error (volatile! method-args)
               reset-all-info (fn []
                                (vreset! next-method-cb-args method-args)
                                (vreset! catched-method-args method-args)
                                (vreset! catched-error method-args))
               gen-fake-method (fn [name]
                                 (fn [& [cb :as args]]
                                   (vswap! catched-method-args
                                           (fn [catched-method-args]
                                             (update catched-method-args name #(conj % args))))
                                   (try
                                     (if (= :execute name)
                                       (apply (nth args 2) (name @next-method-cb-args))
                                       (apply cb (name @next-method-cb-args)))
                                     (catch js/Error err
                                       (js/console.error err)
                                       (vswap! catched-error
                                               (fn [catched-error]
                                                 (update catched-error name #(conj % args))))))))
               fake-error (js/Error. "some error")
               fake-conn (js-obj "beginTransaction" (gen-fake-method :beginTransaction)
                                 "commit" (gen-fake-method :commit)
                                 "rollback" (gen-fake-method :rollback)
                                 "execute" (gen-fake-method :execute))

               _ (vswap! next-method-cb-args
                         (fn [next-method-cb-args]
                           (assoc next-method-cb-args :beginTransaction [fake-error])))
               resp1 (ua/<! (m/with-transaction* {:conn fake-conn :policy :error}
                              (fn []
                                (vswap! catched-method-args
                                        #(update % :callback (fn [args] (conj args []))))
                                (m/exec! fake-conn "hello world"))))
               _ (is (= 1 (count (:beginTransaction @catched-method-args)))
                     "beginTransaction should be called")
               _ (is (= 0 (count (:commit @catched-method-args)))
                     "commit should not be called")
               _ (is (= 0 (count (:rollback @catched-method-args)))
                     "rollback should be called")
               _ (is (= 0 (count (:callback @catched-method-args)))
                     "callback should not be called")
               _ (is (= 0 (count (:execute @catched-method-args)))
                     "execute should not be called")
               _ (is (identical? fake-error resp1))
               _ (reset-all-info)

               _ (vswap! next-method-cb-args
                         (fn [next-method-cb-args]
                           (assoc next-method-cb-args :execute [fake-error])))
               resp2 (ua/<! (m/with-transaction* {:conn fake-conn :policy :error}
                              (fn []
                                (vswap! catched-method-args
                                        #(update % :callback (fn [args] (conj args []))))
                                (m/exec! fake-conn "hello world"))))
               _ (is (= 1 (count (:beginTransaction @catched-method-args)))
                     "beginTransaction should be called")
               _ (is (= 0 (count (:commit @catched-method-args)))
                     "commit should not be called")
               _ (is (= 1 (count (:rollback @catched-method-args)))
                     "rollback should be called")
               _ (is (= 1 (count (:callback @catched-method-args)))
                     "callback should be called")
               _ (is (= 1 (count (:execute @catched-method-args)))
                     "execute should be called")
               _ (is (identical? fake-error resp2))
               _ (reset-all-info)

               _ (vswap! next-method-cb-args
                         (fn [next-method-cb-args]
                           (assoc next-method-cb-args :execute [#js [] #js []])))
               resp3 (ua/<! (m/with-transaction* {:conn fake-conn :policy :error}
                              (fn []
                                (vswap! catched-method-args
                                        #(update % :callback (fn [args] (conj args []))))
                                (m/exec! fake-conn "hello world"))))
               _ (is (= 1 (count (:beginTransaction @catched-method-args)))
                     "beginTransaction should be called")
               _ (is (= 1 (count (:commit @catched-method-args)))
                     "commit should be called")
               _ (is (= 0 (count (:rollback @catched-method-args)))
                     "rollback should not be called")
               _ (is (= 1 (count (:callback @catched-method-args)))
                     "callback should be called")
               _ (is (= 1 (count (:execute @catched-method-args)))
                     "execute should be called")
               _ (is (nil? resp3))
               _ (reset-all-info)]
     (done))))

(deftest with-transaction
  (is (= nil
         (macroexpand-1 '(m/with-transaction {:conn fake-conn}))))
  (is (= '(db.mysql/with-transaction* {:conn fake-conn}
            (clojure.core/fn []
              (utils.async/go (ua/<! (m/exec! "hello")))))
         (macroexpand-1 '(m/with-transaction {:conn fake-conn} (ua/<! (m/exec! "hello")))))))
