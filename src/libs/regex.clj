(ns libs.regex
  (:require [net.cgrand.regex :as re])
  (:use clojure.test)
  (:import (net.cgrand.regex Regex)))

(defn literal-regex [pattern]
  (Regex. pattern nil))

(def regex re/regex)

(defn char-range [& pairs]
  (apply hash-map pairs))

(defn named [name exp]
  (conj [] exp :as name))

(defn named-group [name & exprs]
  (-> exprs vec (conj :as name)))

(defn group [& exprs]
  (vec exprs))

(defn one-of [& exprs]
  (apply hash-set exprs))

(def any re/any)

(def repeated re/repeat)

(defn times [exp count]
  (repeated exp count count))

(def many re/*)

(def at-least-one re/+)

(def maybe re/?)

(def match-re re/exec)

(def al (char-range \a \z \A \Z))

(def alnum (char-range \a \z \A \Z \0 \9))

(def numeric (char-range \0 \9))

(def integer (at-least-one numeric))

(def floating (one-of integer
                      (group (maybe "-")
                             (many numeric)
                             "."
                             (many numeric))))

(def ws (literal-regex #"\s"))

(def non-ws (literal-regex #"\S"))

(deftest test-regexes
  (is (= (match-re
          (regex (named-group :rank (repeated numeric 1 2) (one-of "d" "k")))
          "15k")
         {:rank "15k" nil "15k"})))
