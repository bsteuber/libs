(ns libs.fn
  (:use (libs debug)))

(defn funcall [f arg]
  (f arg))

(defn or-fn [& preds]
  (fn [x]
    (some #(% x)
          preds)))

(defn and-fn [& preds]
  (fn [x]
    (loop [ps preds]
      (let [result ((first ps) x)
            more   (next ps)]
        (if (and result more)
          (recur more)
          result)))))

;;; from David Cabana's blog
;;; http://erl.nfshost.com/2011/05/22/map-mapp-and-mapc/

(defn mapp
  ([f] (partial map f))
  ([f x & args]
     (apply map (partial f x)
            args )))

(defn mapc [& args]
  (let [[fns xs] (partition-by fn? args)
        g (apply comp fns)]
    (if (empty? xs)
      (partial map g)
      (apply map g xs ))))

(defn as-handler
  "Turns a function of 0 or 1 arguments into a 1-argument function that possibly
   ignores it"
  [f]
  (let [arg-count (atom :undef)]
    (fn [arg]
      (case @arg-count
            0 (f)
            1 (f arg)
            :undef (try (let [res (f)]
                          (reset! arg-count 0)
                          res)
                        (catch IllegalArgumentException _
                          (reset! arg-count 1)
                          (try (f arg)
                               (catch IllegalArgumentException _
                                 (fail "Handler function" f
                                       "doesn't accept 0 or 1 args")))))))))

(defmacro with-handlers [handlers & body]
  `(let [~@(->> handlers
                (map (fn [h]
                       (list h `(as-handler ~h))))
                (apply concat))]
     ~@body))
