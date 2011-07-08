(ns test.libs.maps
  (:use [libs.maps :reload true]
        (midje sweet)))

(fact (invert-map {}) => {}
      (invert-map {:a 2}) => {2 :a}
      (invert-map {:a 3, :b 4}) => {3 :a, 4 :b})

(fact (patch-map {} {}) => {}
      (patch-map {:a 1} {}) => {:a 1}
      (patch-map {} {:a 1}) => {:a 1}
      (patch-map {:b 2} {:a 1}) => {:a 1 :b 2}
      (patch-map {:a {:b 1 :c 1}} {:a {:c 2}}) => {:a {:b 1 :c 2}})

(fact (diff-map {} {}) => {}
      (diff-map {} {:a 1}) => {}
      (diff-map {:a 1} {}) => {:a 1}
      (diff-map {:a 1 :b 2} {:a 1}) => {:b 2}
      (diff-map {:a {:b 1 :c 2}} {:a {:b 1}}) => {:a {:c 2}}
      (diff-map {:a {:b 1 :c 2}} {:a {:b 1 :c 2}}) => {})
