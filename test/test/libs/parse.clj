(ns test.libs.parse
  (:use (midje sweet)
        [libs.parse :reload true]))

(fact (parse-int "0") => 0
      (parse-int "815") => 815
      (parse-int "abc[9898]") => nil?)

(fact (parse-float "0") => 0.0
      (parse-float "0.789") => 0.789
      (parse-float ".9") => 0.9
      (parse-float "[6767.2]") => nil?)

(fact (parse-rational "0") => 0
      (parse-rational "0.5") => 1/2)
