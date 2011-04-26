(ns libs.debug
  (:use (libs log))
  (:require [clojure.string :as str]))

(defmacro dbg [x]
  `(let [x# ~x]
     (prn '~x "=" x#)
     x#))

(defn error [& args]
  (apply log :error args)
  (throw (RuntimeException. (str/join " " args))))