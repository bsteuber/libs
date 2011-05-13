(ns libs.dsl)

(defn canonizer [rename]
  (fn canonize [x]
    (cond
     (sequential? x)     (map canonize x)
     (string? x)         x
     (keyword? x)        (rename (name x))
     :default            (rename (str x)))))

(def default-canonize
  (canonizer identity))