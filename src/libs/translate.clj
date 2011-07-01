(ns libs.translate
  (:use (libs log)))

(def ^:dynamic current-dictionary
  (atom nil))

(defn set-dictionary [f]
  (reset! current-dictionary f))

(defn translatable? [key]
  (contains? @current-dictionary key))

(defn descriptive [key]
  (when (keyword? key)
    (keyword (str (name key)
                  "-descr"))))

(defn translate [o]
  (if-not (keyword? o)
    (str o)
    (let [default-result (name o)]
      (if-let [dict @current-dictionary]
        (if-let [translated (dict o)]
          translated
          (do (warn "No translation found for" o)
              default-result))
        default-result))))