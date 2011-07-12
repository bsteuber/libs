(ns libs.parse)

(defn parse-int [s]
  (when-let [x (re-find #"\d+" s)]
    (Integer. x)))
