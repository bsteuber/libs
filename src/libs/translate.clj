(ns libs.translate
  (:use (libs log)))

(def current-dictionary (atom nil))

(defn set-dictionary [f]
  (reset! current-dictionary f))

(defn translate [o & args]
  (if-not (keyword? o)
    (str o)
    (let [default-result (apply print-str (name o) args)]
      (if-let [dict @current-dictionary]
        (if-let [res (apply dict o args)]
          res
          (do (warn "No translation found for" o)
              default-result))
        default-result))))