(ns libs.java.reflect
  (:use (libs debug predicates seq)
        (libs.java gen)))

(defn as-integer [arg]
  (if (instance? Long arg)
    (Integer. arg)
    arg))

(defn find-methods [class pred]
  (filter pred (.getMethods class)))

(defn find-method [class pred]
  (one-element (find-methods class pred)
               (str "Matching methods in class " class)))

(defn name-is? [name]
  #(= (.getName %) name))

(def boxes?          ; TODO float etc.
  #{[Long/TYPE Long]
    [Integer/TYPE Integer]})

(defn arg-match? [clazz arg]
  (let [c (class arg)]
    (or (= c clazz)
        (boxes? [clazz c]))))

(defn all-args-match? [args]
  (fn [m]
    (every? identity
            (map arg-match?
                 (.getParameterTypes m)
                 args))))

(defn applicable-to? [name args]
  (and? (name-is? name)
        (all-args-match? args)))

(defn find-applicable-method [class name args]
  (find-method class
               (applicable-to? name args)))

(defn call-method [obj method & args]
  (if-let [m (find-applicable-method (class obj)
                                     (method-name method)
                                     args)]
    (.invoke m obj (to-array args))
    (fail "No applicable method found")))

(defn call-getter [obj slot-name]
  (call-method obj
        (str "get-" (name slot-name))))

(defn call-setter [obj slot-name value]
  (call-method obj
        (str "set-" (name slot-name))
        value))

(defn call-constructor [clazz & args]
  (let [constructor (->> args
                         (map class)
                         (into-array Class)
                         (.getConstructor clazz))]
    (.newInstance constructor (to-array args))))