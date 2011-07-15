(ns libs.parse)

(defn parse-int [s]
  (try (Integer/parseInt s)
       (catch NumberFormatException _)))

(defn parse-float [s]
  (try (Double/parseDouble s)
       (catch NumberFormatException _)))

(def parse-rational (comp rationalize parse-float))
