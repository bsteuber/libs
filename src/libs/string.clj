(ns libs.string
  (:require [clojure.string :as str]))

(defn vary-first-char [s f]
  (apply str
         (f (first s))
         (rest s)))

