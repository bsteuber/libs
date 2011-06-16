(ns libs.hooks
  (:use (libs debug)))

(defmacro declare-hook [name]
  `(defonce ~name
     (fn [& _#]
       (fail "hook not implemented:" ~(str name)))))

(defmacro register-hook [name args & body]
  `(alter-var-root (var ~name)
                   (constantly (fn ~args ~@body))))
