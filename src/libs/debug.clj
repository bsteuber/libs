(ns libs.debug
  (:use (libs log))
  (:require [clojure.string :as str]))

(defmacro dbg [x]
  `(let [x# ~x]
     (println '~x "=" x#)
     x#))

(defn fail [& args]
  (apply error args)
  (throw (RuntimeException. (str/join " " args))))