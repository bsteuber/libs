(ns libs.imperative)

(defn domap
  "Like map, but only used for side effects - returns nil"
  [f seq]
  (doseq [elt seq]
    (f elt)))

(defmacro dolet
  "Like a let for only one variable, which is returned in the end."
  [[var expr] & cmds]
  `(let [~var ~expr]
     ~@cmds
     ~var))