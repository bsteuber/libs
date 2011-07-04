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

(fact (parse-options [:a :b] [])               => [{} []])
(fact (parse-options [:a :c] [:a 1 :b 2 :a 3]) => [{:a 1} [:b 2 :a 3]])

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

(deff deff-tester [title text args & other-args]
  (let [[x y] args]
    [title text x y other-args]))

(fact (deff-tester)                     => [nil nil nil nil []]
      (deff-tester 5)                   => [nil nil 5 nil []]
      (deff-tester 4 5)                 => [nil nil 4 5 []]
      (deff-tester :title :foo)         => [:foo nil nil nil []]
      (deff-tester :text :bar)          => [nil :bar nil nil []]
      (deff-tester 4 5 :title :baz 6 7) => [:baz nil 4 5 [:args [6 7]]])