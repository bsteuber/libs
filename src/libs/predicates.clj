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

(defn all? [xs]
  (every? identity xs))

(defn and? [& preds]
  (fn [x]
    (loop [ps preds]
      (let [result ((first ps) x)
            more   (next ps)]
        (if (and result more)
          (recur more)
          result)))))


