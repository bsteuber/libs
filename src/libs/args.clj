(ns libs.args
  (:use (libs predicates)))

(defn parse-last-arg [args]
  (if (even? (count args))
    args
    [(last args) (butlast args)]))

(defn parse-options [option-keys args]
  (let [paired-args (partition 2 args)
        keys        (into #{} option-keys)
        take?       (first? keys)
        options     (apply hash-map
                           (apply concat
                                  (filter take? paired-args)))
        other-args  (apply concat (remove take? paired-args))]
    [options other-args]))