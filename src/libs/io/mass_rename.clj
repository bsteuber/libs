(ns io.mass-rename
  (:use clojure.java.io
        clojure.contrib.core)
  (:require [clojure.string :as str]))

(defn transform-filename [in-file f]
  (let [path (str/split (str in-file)
                        #"/")
        [name dirname & rest] (reverse path)
        res (f dirname name)]
    (if res
      {:in in-file
       :out (file (str/join "/" (concat (reverse rest)
                                         [dirname res])))})))

(defn mass-rename [dir rename-fn & [do-it?]]
  (doseq [{:keys [in out]}
          (remove nil? (map #(transform-filename % rename-fn)
                            (.listFiles (file dir))))]
    (when-not (= (str in) (str out))
      (println "mv " (str in))
      (println "to " (str out))
      (println)
      (when do-it?
        (copy in out)
        (delete-file in)))))

;;; Renaming sub-titles

(defn rename-sub [season name]
  (let [re #".*Foo.*(\d\d).*(\d\d).*srt"
        ms (re-matches re name)]
    (if ms
      (str season (nth ms 2) ".srt"))))

(comment
  (mass-rename "/some/season/" rename-sub))