(ns mysql.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require ["mysql2" :as m]
            ["lodash.isobjectlike" :as is-object-like]
            [cljs.core.async :as async]
            [utils.core :as uc]
            [utils.async :as ua]))

(defn- normalize->clj [a & opts]
  (-> (js/JSON.stringify a)
      js/JSON.parse
      (#(apply js->clj % opts))))

(defn conn
  "Create mysql connection
  Support both url string or hash:

  Example:

  `(conn \"mysql://user:pass@host/db?debug=true&charset=BIG5_CHINESE_CI&timezone=-0700\")`

  `(conn :user \"root\" :passworld \"\")`

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
