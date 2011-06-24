(ns libs.binding
  (:import (clojure.lang IPersistentMap
                         IPersistentVector
                         ISeq
                         Symbol)))

(defmulti transform-binding (fn [lhs rhs]
                              (type lhs)))

(defmethod transform-binding :default
  [lhs rhs]
  [lhs rhs])

(defmethod transform-binding ISeq
  [[fname & args] rhs]
  [fname `(fn [~@args]
            ~rhs)])

(defn transform-bindings [bindings]
  (->> bindings
       (partition 2)
       (mapcat (fn [[lhs rhs]]
                 (transform-binding lhs rhs)))
       vec))

(defmacro bind [bindings & body]
  `(let ~(transform-bindings bindings)
     ~@body))

(defmacro where [ret & bindings]
  `(bind ~bindings
     ~ret))

(defmacro with [& args]
  `(bind  ~(butlast args)
     ~(last args)))

;; ;; TODO docstring etc.
;; (defmacro def* [lhs & args]
;;   (where `(def ~name
;;             ~rhs)
;;     [meta-args rhs] (loop [forms args
;;                            meta-args ()]
;;                       (with [next-arg & more-args]  )
;;                  )
;;     [name rhs] (transform-binding lhs rhs)))
