(ns test.libs.maps
  (:use [libs.maps :reload true]
        (midje sweet)))

(fact (invert-map {}) => {}
      (invert-map {:a 2}) => {2 :a}
      (invert-map {:a 3, :b 4}) => {3 :a, 4 :b})
