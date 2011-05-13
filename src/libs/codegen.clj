(ns libs.codegen
  (:require [clojure.string :as str]))

(def endl "\n")

(def ^:dynamic *indent* "  ")

(def indent
  #(.replace % endl (str endl *indent*)))

(defn embracer
  ([open]       (embracer open open))
  ([open close] #(str open % close)))

(defn joiner [seperator]
  #(str/join seperator %&))

(defn applier [f]
  #(apply f %))

(def prepend #(str %2 %1))
(def append  str)

(def parens   (embracer "(" ")"))
(def braces   (embracer "{" "}"))
(def brackets (embracer "[" "]"))

(def words      (joiner " "))
(def lines      (joiner "\n"))
(def paragraphs (joiner "\n\n"))
(def commas     (joiner ", "))
(def semicolons (joiner "; "))

(def words*      (applier words))
(def lines*      (applier lines))
(def paragraphs* (applier paragraphs))
(def commas*     (applier commas))
(def semicolons* (applier semicolons))

(defn block [cmds]
  (-> cmds
      lines*
      (prepend endl)
      indent
      (append endl)))
