(ns libs.char)

(defn upper-case? [char]
  (Character/isUpperCase char))

(defn lower-case? [char]
  (Character/isLowerCase char))

(defn upper-case [char]
  (Character/toUpperCase char))

(defn lower-case [char]
  (Character/toLowerCase char))

