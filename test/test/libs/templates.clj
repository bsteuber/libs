(ns test.libs.templates
  (:use (midje sweet)
        (libs parse)
        [libs.regex :only [integer]]
        [libs.templates :reload true]))

(fact (parse-from-template ["Hello" :world] "Hello World") => {:world "World"}
      (parse-from-template ["x:" [:x integer]] "x:569") => {:x "569"}
      (parse-from-template ["x:" [:x integer (comp inc parse-int)]] "x:99") => {:x 100}
      (parse-from-template [(as-int :x) (as-float :y)] "42 0.17") => {:x 42, :y 0.17}
      (parse-from-template ["x:" (list :foo :bar)] "x:99 11") => {:foo "99" :bar "11"}
      (parse-from-template [(in-brackets (as-int :x))] "[1234]") => {:x 1234}
      (parse-from-template [(in-brackets (in-parens :foo))] "[(hoho)]") => {:foo "hoho"})
