(ns db.mysql-test-delay
  (:refer-clojure :exclude [uuid])
  (:require
   ["dotenv-safe/config"]
   [pjstadig.humane-test-output]
   [cljs.test :as ct :refer [deftest testing is]]
   [utils.core :as uc]
   [utils.async :as ua]
   [db.mysql :as m]
   ["mysql2/lib/constants/types" :as DBTypes]
   ["mysql2/lib/constants/field_flags" :as FieldFlags]
   ["lodash.isequal" :as js-equal]
   ["uuid/v4" :as uuid]))

(defn normalize-field [o]
  (-> (js/JSON.stringify o)
      js/JSON.parse
      (js-delete "_buf")))

(def conn (m/conn js/process.env.MYSQL_URL))

(deftest exec!
  (ct/async
   done
   (ua/go-let
     [bookname (uuid)
      res (ua/<! (ua/go-try-let
                   [_ (ua/<? (m/exec! conn "DROP TABLE IF EXISTS Books"))
                    _ (ua/<? (m/exec! conn "
CREATE TABLE Books (
  id INT(6) UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  name VARCHAR(100) NOT NULL
)
"))
                    e1 (ua/<? (m/exec! conn "SELECT * FROM Books"))
                    e2 (ua/<? (m/exec! conn (str "INSERT INTO Books (name) VALUE(\"" bookname "\")")))
                    e3 (ua/<? (m/exec! conn "SELECT * FROM Books"))]
                   [e1 e2 e3]))
      [e1 e2 e3] (if (uc/error? res)
                   (do (js/console.error res)
                       [nil nil nil])
                   res)]
     (when (uc/error? res)
       (js/console.error res))
     (is (not (uc/error? res)))

     (is (= [] (:rows e1)))
     (is (= 2 (count (:fields e1))))
     (is (= "id" (.-name (first (:fields e1)))))
     (is (= (.-LONG DBTypes) (.-columnType (first (:fields e1)))))
     (is (bit-and (.-UNSIGNED FieldFlags) (.-flags (first (:fields e1)))))
     (is (bit-and (.-PRI_KEY FieldFlags) (.-flags (first (:fields e1)))))
     (is (bit-and (.-AUTO_INCREMENT FieldFlags) (.-flags (first (:fields e1)))))
     (is (bit-and (.-PRI_KEY FieldFlags) (.-flags (first (:fields e1)))))
     (is (= "name" (.-name (second (:fields e1)))))
     (is (= (.-VAR_STRING DBTypes) (.-columnType (second (:fields e1)))))
     (is (bit-and (.-NOT_NULL FieldFlags) (.-flags (second (:fields e1)))))
     (is (nil? (:resultHeader e1)))

     (is (nil? (:rows e2)))
     (is (nil? (:fields e2)))
     (is (= 1 (.-affectedRows (:resultHeader e2))))

     (is (= [{:id 1 :name bookname}] (:rows e3)))
     (is (= (count (:fields e1)) (count (:fields e3))))
     (is (js-equal (normalize-field (first (:fields e1))) (normalize-field (first (:fields e3)))))
     (is (js-equal (normalize-field (second (:fields e1))) (normalize-field (second (:fields e3)))))
     (is (nil? (:resultHeader e3)))

     (done))))

(deftest with-transaction*
  (ct/async
   done
   (ua/go-let
     [bookname (uuid)
      fake-error (js/Error. "test error")
      _ (ua/<? (m/exec! conn "DROP TABLE IF EXISTS Books"))
      _ (ua/<? (m/exec! conn "
CREATE TABLE Books (
  id INT(6) UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  name VARCHAR(100) NOT NULL
)"))
      query-in-transaction (volatile! nil)
      res (ua/<! (m/with-transaction* conn
                   (fn []
                     (ua/go-try
                      (ua/<? (m/exec! conn "SELECT * FROM Books"))
                      (ua/<? (m/exec! conn (str "INSERT INTO Books (name) VALUE(\"" bookname "\")")))
                      (vreset! query-in-transaction (ua/<? (m/exec! conn "SELECT * FROM Books")))
                      (throw fake-error)))))
      query-out-transaction (ua/<! (m/exec! conn "SELECT * FROM Books"))]
     (is (identical? fake-error res))
     (is (= bookname (:name (first (:rows @query-in-transaction)))))
     (is (= [] (:rows query-out-transaction)))
     (done))))
