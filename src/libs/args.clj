(ns libs.args
  (:use (libs predicates)))

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

(defn parse-options [option-keys args]
  (let [take-key? (into #{} option-keys)]
    (loop [remaining-args args
           options        {}
           unread-args    []]
      (if-not (seq remaining-args)
        [options unread-args]
        (let [[key val & more] remaining-args]
          (if (and (take-key? key)
                   (not (contains? options key)))
            (recur more (assoc options key val) unread-args)
            (recur more options (conj unread-args key val))))))))


;; TODO docstring etc.
(defmacro deff
  "Defines a function with flexible keyword argument parsing"
  [name args & body]
  (let [[and-sym rest-args-sym] (take-last 2 args)
        [key-args rest-args-sym] (if (= and-sym '&)
                                   [(drop-last 2 args) rest-args-sym]
                                   [args               (gensym)])]
    `(defn ~name [& args#]
       (let [[{:keys ~key-args}
              ~rest-args-sym] (parse-options
                               ~(vec (map keyword key-args))
                               (parse-args args#))]
         ~@body))))

