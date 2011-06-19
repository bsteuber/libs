(ns libs.thread
  (:use (libs debug)))

(defmacro spawn [& body]
  `(do (future (try ~@body
                    (catch Throwable e#
                      (handle-error e#))))
       nil))


