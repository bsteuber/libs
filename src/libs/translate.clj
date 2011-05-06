(ns libs.translate
  (:use (libs log)))

(def current-dictionary (atom nil))

(defn load-dictionary [file])

(defn translate [key-or-str & args]
  (let [not-translated-result (apply print-str key-or-str args)]
    (if (keyword? key-or-str)
      (if-let [dict @current-dictionary]
        (if-let [res (apply dictionary key-or-str args)]
          res
          (do (warn "No translation found for" not-translated-result)
              not-translated-result))
        (do (warn "No dictionary loaded!")
            not-translated-result))
      not-translated-result)))