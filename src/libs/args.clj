(ns libs.args
  (:use (libs predicates)))

(defn process-content-arg [args]
  (if (even? (count args))
    args
    (concat (butlast args)
            [:content, (last args)])))

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
                                    (process-content-arg ~args-sym))]
     ~@body))