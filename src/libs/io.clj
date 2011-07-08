(ns libs.io
  (:use (clojure.java io))
  (:require [clojure.string :as str]))

(defn read-as-str [stream]
  (->> stream
       reader
       line-seq
       (str/join "\n")))

(defn serialize [file data]
  (.mkdirs (.getParentFile file))
  (binding [*out* (writer file)]
    (prn data)))

(defn deserialize [filename]
  (read-string (try (slurp filename)
                    (catch Exception _ "nil"))))
