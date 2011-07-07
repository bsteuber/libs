(ns test.libs.fn
  (:use [libs.fn :reload true]
        midje.sweet))

(fact ((as-handler #(+ 1 1)) 42) => 2
      ((as-handler #(+ 1 %)) 42) => 43)

(fact (let [h #(+ 1 1)
            i #(+ 1 %)]
        (with-handlers [h i]
          [(h 42) (i 42)])) => [2 43])
