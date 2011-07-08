(ns libs.def)

(defmacro redef [name & fn-args]
  `(alter-var-root (var ~name)
                   (constantly (fn ~@fn-args))))
