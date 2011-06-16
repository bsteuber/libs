(ns libs.debug
  (:use (clojure stacktrace)
        (libs log))
  (:require [clojure.string :as str]))

(defmacro dbg [x]
  `(let [x# ~x]
     (println '~x "=" x#)
     x#))

(defn fail [& args]
  (apply error args)
  (throw (RuntimeException. (str/join " " args))))

;; Overwrite this using a hook
(defn handle-error [e]
  (let [trace ((with-out-str (print-cause-trace e)))]
    (fail trace)))

(defmacro future* [& body]
  `(future (try ~@body
                (catch Throwable e#
                  (handle-error e#)))))
