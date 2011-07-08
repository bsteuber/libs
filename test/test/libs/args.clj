(ns test.libs.args
  (:use libs.args
        midje.sweet))

(fact (parse-args [])                  => []
      (parse-args [42])                => [:args [42]]
      (parse-args [[1 2 3]])           => [:args [1 2 3]]
      (parse-args [[[1 2 3]]])         => [:args [[1 2 3]]]
      (parse-args [:a 1])              => [:a 1]
      (parse-args [:b 2 :foo])         => [:b 2 :args [:foo]]
      (parse-args [:c 3 4 5])          => [:c 3 :args [4 5]]
      (parse-args [[] :a 4 [[]] 5 :b]) => [:args [] :a 4 :args [[]] :args [5 :b]]
      (parse-args [:a 1 [:x :y] :z 6]) => [:a 1 :args [:x :y] :z 6])

(fact (parse-options [:a :b] [])               => [{} []])
(fact (parse-options [:a :c] [:a 1 :b 2 :a 3]) => [{:a 1} [:b 2 :a 3]])

(deff deff-tester [title text args & other-args]
  (let [[x y] args]
    [title text x y other-args]))

(fact (deff-tester)                     => [nil nil nil nil []]
      (deff-tester 5)                   => [nil nil 5 nil []]
      (deff-tester 4 5)                 => [nil nil 4 5 []]
      (deff-tester :title :foo)         => [:foo nil nil nil []]
      (deff-tester :text :bar)          => [nil :bar nil nil []]
      (deff-tester 4 5 :title :baz 6 7) => [:baz nil 4 5 [:args [6 7]]])