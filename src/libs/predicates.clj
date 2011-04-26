(ns libs.predicates)

(defn eq? [val]
  #(= % val))

(def one-of? hash-set)

(defn first? [pred]
  (comp pred first))

(defn second? [pred]
  (comp pred second))

(defn rest? [pred]
  (comp pred rest))

(def else?
  (constantly true))

(defn or? [& preds]
  (fn [x]
    (some #(% x)
          preds)))

(defn and? [& preds]
  (fn [x]
    (when (every? #(% x)
                  preds)
      ((last preds) x))))     ;; inefficient