(ns libs.io
  (:use [clojure.java.io :only [file reader writer]])
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

(defn make-dir [& file-args]
  (doto (apply file file-args)
    (.mkdir)))

(defn make-file
  ([path]
     (make-file "." path))
  ([root path]
     (let [path-tokens (str/split path #"/")
           dirs        (butlast path-tokens)
           filename    (last path-tokens)]
       (file (reduce make-dir root dirs)
             filename))))
