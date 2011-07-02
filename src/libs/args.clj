(ns libs.args
  (:use (libs predicates)
        (midje sweet)))

(defn parse-args [args]
  (loop [args        args
         parsed-args []]
    (if-not (seq args)
      parsed-args
      (let [[fst & more-1] args
            [snd & more-2] more-1]
        (cond
         (and (keyword? fst)
              more-1)
         (recur more-2
                (conj parsed-args fst snd))
         ;;
         (sequential? fst)
         (recur more-1
                (conj parsed-args :args fst))
         ;;
         :else
         (let [[eatable
                non-eatable] (split-with #(not (or (keyword? %)
                                                   (sequential? %)))
                                         args)]
           (cond
            (not non-eatable)
            (conj parsed-args :args eatable)
            ;;
            (and (keyword? (first non-eatable))
                 (not (next non-eatable)))
            (conj parsed-args
                  :args (conj (vec eatable)
                              (first non-eatable)))
            ;;
            :else
            (recur non-eatable
                   (conj parsed-args :args eatable)))))))))

(fact (parse-args [])                  => []
      (parse-args [42])                => [:args [42]]
      (parse-args [[1 2 3]])           => [:args [1 2 3]]
      (parse-args [[[1 2 3]]])         => [:args [[1 2 3]]]
      (parse-args [:a 1])              => [:a 1]
      (parse-args [:b 2 :foo])         => [:b 2 :args [:foo]]
      (parse-args [:c 3 4 5])          => [:c 3 :args [4 5]]
      (parse-args [[] :a 4 [[]] 5 :b]) => [:args [] :a 4 :args [[]] :args [5 :b]]
      (parse-args [:a 1 [:x :y] :z 6]) => [:a 1 :args [:x :y] :z 6])

(defn parse-options [option-keys args]
  (let [paired-args (partition 2 args)
        keys        (into #{} option-keys)
        take?       (first? keys)
        options     (apply hash-map
                           (apply concat
                                  (filter take? paired-args)))
        other-args  (apply concat (remove take? paired-args))]
    [options other-args]))

(defmacro with-options [[keys args-sym] & body]
  `(let [[{:keys ~keys}
          ~args-sym] (parse-options ~(vec (map keyword keys))
                                    (parse-args ~args-sym))]
     ~@body))