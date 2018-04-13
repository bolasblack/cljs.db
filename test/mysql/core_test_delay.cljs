(ns mysql.core-test-delay
  (:require
   ["dotenv-safe/config"]
   [pjstadig.humane-test-output]
   [cljs.test :as ct :refer [deftest testing is]]
   [utils.core :as uc]
   [utils.async :as ua]
   [mysql.core :as c]
   ["mysql2/lib/constants/types" :as DBTypes]
   ["mysql2/lib/constants/field_flags" :as FieldFlags]
   ["lodash.isequal" :as js-equal]))

(defn normalize-field [o]
  (-> (js/JSON.stringify o)
      js/JSON.parse
      (js-delete "_buf")))

(def conn (c/conn js/process.env.MYSQL_URL))

(deftest exec!
  (ct/async
   done
   (ua/go-let
     [res (ua/<! (ua/go-try-let
                   [_ (ua/<? (c/exec! conn "DROP TABLE IF EXISTS Books"))
                    _ (ua/<? (c/exec! conn "
CREATE TABLE Books (
  id INT(6) UNSIGNED AUTO_INCREMENT PRIMARY KEY,
  name VARCHAR(30) NOT NULL
)
"))
                    e1 (ua/<? (c/exec! conn "SELECT * FROM Books"))
                    e2 (ua/<? (c/exec! conn "INSERT INTO Books (name) VALUE(\"Book1\")"))
                    e3 (ua/<? (c/exec! conn "SELECT * FROM Books"))]
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

     (is (= [{:id 1 :name "Book1"}] (:rows e3)))
     (is (= (count (:fields e1)) (count (:fields e3))))
     (is (js-equal (normalize-field (first (:fields e1))) (normalize-field (first (:fields e3)))))
     (is (js-equal (normalize-field (second (:fields e1))) (normalize-field (second (:fields e3)))))
     (is (nil? (:resultHeader e3)))

     (done))))
