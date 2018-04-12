(ns mysql.core
  (:require [utils.async :as ua :include-macros true]))

(defmacro with-transaction
  [opts & body]
  (if (empty? body)
    nil
    `(with-transaction* ~opts
       (fn []
         (ua/go ~@body)))))
