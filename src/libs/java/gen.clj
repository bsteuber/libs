(ns libs.java.gen
  "Utilities for generating Java classes from clojure"
  (:use (libs char debug string))
  (:require [clojure.string :as str]))

(defn class-name [s]
  (->> (.split (name s) "-")
       (map str/capitalize)
       str/join))

(defn method-name [s]
  (vary-first-char (class-name s)
                   lower-case))

(def constant-name
  (comp str/upper-case
        #(.replace % "-" "_")))