(ns libs.thread
  (:use (libs debug)))

(defmacro spawn [& body]
  `(future (try ~@body
                (catch Throwable e#
                  (handle-error e#)))))

(defn handle-all-threads []
  (Thread/setDefaultUncaughtExceptionHandler
   (proxy [Thread$UncaughtExceptionHandler] []
       (uncoughtException [thread e] (handle-error e)))))
