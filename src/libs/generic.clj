(ns libs.generic
  (:use (libs.java reflect))
  (:refer-clojure :exclude [get set])
  (:require [libs.java.meta :as m]))

(defmulti as (fn [clazz o]
               [clazz (m/type o)]))

(defmulti get (fn [o key]
                [(m/type o) key]))

(defmulti set (fn [o key value]
                [(m/type o) key]))

(defmulti on (fn [o key handler]
               [(m/type o) key]))

(defn make [clazz & args]
  (as clazz args))

(defmethod get :default
  [o key]
  (call-getter o key))

(defmethod set :default
  [o key value]
  (call-setter o key value))

(defmethod set [Object :on]
  [o key handlers]
  (doseq [[key handler] (if (map? handlers)
                          handlers
                          (partition 2 handlers))]
    (on o key handler)))

(defn set-all [o kv-pairs]
  (doseq [[k v] kv-pairs]
    (set o k v))
  o)

(defn update [o key f & args]
  (set o key (apply f (get f key) args)))

(defmethod as [Object clojure.lang.IPersistentMap]
  [clazz m]
  (set-all (call-constructor clazz)
           m))

(defmethod as [Object clojure.lang.Sequential]
  [clazz seq]
  (set-all (call-constructor clazz)
           (partition 2 seq)))

