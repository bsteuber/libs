(ns libs.maps
  (:use (midje sweet)))

(defn invert-map [m]
  (into {} (map (fn [[x y]]
                  [y x])
                m)))

(fact (invert-map {}) => {}
      (invert-map {:a 2}) => {2 :a}
      (invert-map {:a 3, :b 4}) => {3 :a, 4 :b})
