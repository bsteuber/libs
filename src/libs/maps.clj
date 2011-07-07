(ns libs.maps)

(defn invert-map [m]
  (into {} (map (fn [[x y]]
                  [y x])
                m)))
