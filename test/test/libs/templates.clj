(ns test.libs.templates
  (:use (midje sweet)
        (libs parse)
        [libs.regex :only [integer]]
        [libs.templates :reload true]))

(fact (parse-from-template ["Hello" :world] "Hello World") => {:world "World"}
      (parse-from-template ["x:" [:x integer]] "x:569") => {:x "569"}
      (parse-from-template ["x:" [:x integer (comp inc parse-int)]] "x:99") => {:x 100})
