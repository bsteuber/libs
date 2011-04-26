(ns libs.generic
  (:use (libs.java reflect))
  (:refer-clojure :exclude [get set]))

(defmulti as (fn [clazz o]
               [clazz (class o)]))

(defmulti get (fn [o key]
                [(class o) key]))

(defmulti set (fn [o key value]
                [(class o) key]))

(defmethod set :default
  [o key value]
  (call-setter o key value))

(defn set-all [o kv-pairs]
  (doseq [[k v] kv-pairs]
    (set o k v))
  o)

(defn update [o key f & args]
  (set o key (apply f (get f key) args)))

(defmethod as [Object clojure.lang.ILookup]
  [clazz m]
  (set-all (call-constructor clazz)
           m))

(defmulti on (fn [o key handler]
               [(class o) key]))