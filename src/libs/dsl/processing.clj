(ns libs.dsl.processing
  (:use (libs codegen
              dsl
              javagen
              fn
              string)
        (libs.codegen java)))

(defmacro cond-match [&args] nil)

(def cast?        (eq? "cast"))
(def bool-op?     (one-of? "==" "<" ">" "<=" ">="))
(def op?          (one-of? "+" "-" "*" "/" "=" "&&" "||"))
(def postfix-op?  (one-of? "++" "--"))
(def primitive?   (one-of? "void" "int"))
(def class-name?  (first? upper-case?))
(def constructor? (and? seq?
                        (first? class-name?)))

(defn rename-java [s]
  s
  (if (= s "-")
    s
    (java-name s)))      

(def canonize-java (canonizer rename-java))

(defn compile-expr [expr]
  (if (string? expr)
    expr
    (apply
     (condp call (first expr)       
       cast?               #(gen-cast %2)
       bool-op?            #(gen-bool-op %1 %&)
       op?                 #(gen-op %1 %&)
       postfix-op?         gen-postfix-op
       primitive?          (if (seq? (second expr))
                              #(gen-fun-def %1 (first %2) (rest %2) %&)
                              gen-var-def)
       constructor?        #(gen-constructor (first %1) (rest %1) %&)       
       else?               #(gen-fun-call %1 %&))
     expr)))

(defn compile-cmds [cmds]
  (->> cmds
      (map compile-expr)
      lines*))

(defn sketch [cmds]
  (compile-cmds (canonize-java cmds)))

