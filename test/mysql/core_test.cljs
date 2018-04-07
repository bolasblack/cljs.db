(ns mysql.core-test
  (:require
   [pjstadig.humane-test-output]
   [cljs.test :as ct :refer [deftest testing is]]
   [utils.async :as ua]
   ["mysql2" :as m]
   [mysql.core :as c]
   ["lodash.isequal" :as js-equal]))

(deftest conn
  (with-redefs [m/createConnection (fn [& args] args)]
    (is (= '("conn string") (c/conn "conn string")))
    (is (= [{"conn-param1" 1 "conn-param2" 2}]
           (js->clj (c/conn :conn-param1 1 :conn-param2 2))))))

(deftest close!
  (let [fake-conn-called (volatile! false)
        fake-conn #js {:close #(vreset! fake-conn-called true)}]
    (c/close! fake-conn)
    (is @fake-conn-called)))

(deftest end!
  (ct/async
   done
   (ua/go-let [fake-error (js/Error.)
               fake-conn-call-time (volatile! 0)
               fake-conn1 (js-obj "end" #(do (vswap! fake-conn-call-time inc)
                                             (%)))
               resp1 (ua/<! (c/end! fake-conn1))
               fake-conn2 (js-obj "end" #(do (vswap! fake-conn-call-time inc)
                                             (% fake-error)))
               resp2 (ua/<! (c/end! fake-conn2))]
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
               resp1 (ua/<! (c/exec! fake-conn1 "sql string" ["sql args1" :sql-args2]))
               resp2 (ua/<! (c/exec! fake-conn1 "sql string"))
               fake-conn2 (js-obj "execute" (fn [& [_ _ callback :as args]]
                                              (vswap! fake-conn-args #(conj % args))
                                              (callback nil #js [#js {:id 1} #js {:id 2}] #js {:fake-fields true})))
               resp3 (ua/<! (c/exec! fake-conn2 {:sql "sql string" :timeout 5000} ["sql args1" :sql-args2]))]
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
     (is (= {:rows [{:id 1} {:id 2}]
             :fields {:fake-fields true}}
            resp3))

     (done))))

     (done))))
