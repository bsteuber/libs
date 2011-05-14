(ns libs.translate
  (:use (libs log)))

(def current-dictionary (atom nil))

(defn set-dictionary [f]
  (reset! current-dictionary f))

(defn translate [key-or-str & args]
  (let [not-translated-result (apply print-str (name key-or-str) args)]
    (if (keyword? key-or-str)
      (if-let [dict @current-dictionary]
        (if-let [res (apply dict key-or-str args)]
          res
          (do (warn "No translation found for" key-or-str)
              not-translated-result))
        (do (warn "No dictionary loaded!")
            not-translated-result))
      not-translated-result)))