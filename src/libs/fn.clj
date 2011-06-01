(ns libs.fn)

(defn funcall [f arg]
  (f arg))

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
