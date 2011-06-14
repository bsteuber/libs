(ns libs.io
  (:use (clojure.java io))
  (:require [clojure.string :as str]))

(defn read-as-str [stream]
  (->> stream
       reader
       line-seq
       (str/join "\n")))

