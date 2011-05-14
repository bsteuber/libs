(ns libs.generic
  (:refer-clojure :exclude [get set])
  (:use (libs debug)
        (libs.java reflect))
  (:require [clojure.core :as core]
            [libs.java.meta :as m])
  (:import (clojure.lang IDeref
                         ILookup
                         IPersistentMap
                         IPersistentSet
                         Sequential)))

(defmulti as (fn [clazz o]
               [clazz (m/type o)]))

(defmulti get (fn [o key]
                [(m/type o) key]))

(defmulti get1 (fn [o key]
                 (m/type o)))

(defmulti set (fn [o key value]
                [(m/type o) key]))

(defmulti set1 (fn [o key value]
                 (m/type o)))

(defmulti on (fn [o key handler]
               [(m/type o) key]))

(defn make [clazz & args]
  (as clazz args))

(defmethod get :default
  [o key]
  (get1 o key))

(defmethod get1 :default
  [o key]
  (call-getter o key))

(defmethod get1 ILookup
  [o key]
  (core/get o key))

(defmethod get1 IPersistentSet
  [o key]
  (core/get o key))

(defmethod get1 IDeref
  [o key]
  (get @o key))

(defmethod set :default
  [o key value]
  (set1 o key value))

(defmethod set1 :default
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

(defmethod as [Object IPersistentMap]
  [clazz m]
  (set-all (call-constructor clazz)
           m))

(defmethod as [Object Sequential]
  [clazz seq]
  (set-all (call-constructor clazz)
           (partition 2 seq)))

(def ^:dynamic *backends* {:ui :swing})

(defn set-backend [frontend backend]
  (alter-var-root #'*backends* assoc frontend backend))

(defmulti backend-class (fn [backend name]
                          [backend name]))

(defmethod backend-class :default
  [backend type]
  (fail "No implementation for type" type "in backend" backend))

(defmacro def-backend [backend-name & args]
  `(do ~@(for [[name class] (partition 2 args)]
           `(defmethod backend-class [~backend-name ~(keyword name)]
              [_# _#]
              ~class))))

(defmacro def-frontend [frontend-name & fnames]
  `(do ~@(for [name fnames]
           `(defn ~name [& args#]
              (as (backend-class (*backends* ~frontend-name) ~(keyword name))
                  args#)))))
