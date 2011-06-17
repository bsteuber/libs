(ns libs.thread
  (:use (libs debug)))

(defmacro spawn [& body]
  `(future (try ~@body
                (catch Throwable e#
                  (handle-error e#)))))
