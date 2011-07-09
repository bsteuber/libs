(ns libs.multi)

(defmacro with-multi [[multi & method-defs] & body]
  (let [method-defs (map #(cons (gensym) %)
                         method-defs)]
    `(let [~@(apply concat
                    (for [[old key] method-defs]
                      [old
                       `(get-method ~multi ~key)]))]
       ~@(for [[_ key & defmethod-args] method-defs]
           `(defmethod ~multi ~key ~@defmethod-args))
       (let [res# (do ~@body)]
         ~@(for [[old key] method-defs]
             `(if ~old
                (.addMethod ~multi ~key ~old)
                (remove-method ~multi ~key)))
         res#))))
