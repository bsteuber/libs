(ns libs.def)

(defmacro redef [name val]
  `(alter-var-root (var ~name)
                   (constantly ~val)))

(defmacro redefn [name & fn-args]
  `(redef ~name (fn ~@fn-args)))
