(ns libs.codegen.java
  (:use (libs codegen seq)))

(def java-block
  (comp braces block))

(defn gen-cmd [expr]
  (append expr ";"))

(defn gen-return [expr]
  (words "return" expr))

(defn gen-if [pred cmds]
  (words "if"
         (parens pred)
         (java-block cmds)))

(defn gen-else [cmds]
  (words "else" 
         (java-block cmds)))

(defn gen-else-if [cmds]
  (words "else if"
         (java-block cmds)))

(defn gen-for [init condition step cmds]
  (words "for"
         (parens (semicolons init condition step))
         (java-block cmds)))

(defn gen-fun-call [name args]
  (str name
       (parens (commas* args))))

(defn gen-constructor [name args cmds]
  (words (gen-fun-call name args)              
         (java-block cmds)))

(defn gen-fun-def [type name args cmds]
  (words type
         (gen-fun-call name args)              
         (java-block cmds)))

(defn gen-class-def [name cmds]
  (words "class"
         name
         (java-block cmds)))

(defn gen-var-def
  ([type name]
     (words type name))
  ([type name init]
     (words type name "=" init)))

(defn gen-op [op args]
  (parens
   (words* (interpose op args))))

(defn gen-bool-op [op args]
  (if (= (count args)
         2)
    (gen-op op args)          
    (let [arg-pairs (partition 2 1 args)]
      (gen-op "&&"
              (map #(gen-op op %)
                   arg-pairs)))))

(defn gen-postfix-op [op x]
  (parens (str x op)))

(defn gen-cast [type expr]
  (words (parens type)
         expr))