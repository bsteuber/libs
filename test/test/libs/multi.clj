(ns test.libs.multi
  (:use (midje sweet)
        [libs.multi :reload true]))

(defmulti foo identity)

(defmethod foo :bar [x] :bar)

(fact (foo :bar) => :bar)

(with-multi [foo
             [:bar [x] :mocked-bar]
             [:undef [x] :mocked-undef]]
  (fact (foo :bar) => :mocked-bar)
  (fact (foo :undef) => :mocked-undef))

(fact (foo :bar)   => :bar)
(fact (foo :undef) => (throws IllegalArgumentException))
