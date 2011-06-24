(ns libs.generic
  (:refer-clojure :exclude [get set])
  (:use (libs args debug log)
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

(defn value [x]
  (get x :value))

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

(defn update [o key f & args]
  (set o key (apply f (get f key) args)))

(defn set-all [o kv-pairs]
  (doseq [[k v] (partition 2 (process-content-arg kv-pairs))]
    (try (set o k v)
         (catch Exception e
           (error "Error in generic setter:"
                 "\n  slot:       " k
                 "\n  class:      " (class o)
                 "\n  value:      " v
                 "\n  value class:" (class v))
           (throw e))))
  o)

(defn make [clazz & args]
  (as clazz args))

(defn conf [o & args]
  (set-all o args))

(defmethod as [Object IPersistentMap]
  [clazz m]
  (set-all (call-constructor clazz)
           m))

(defmethod as [Object nil]
  [clazz _]
  (call-constructor clazz))

(defmethod as [Object Sequential]
  [clazz keyvals]
  (set-all (call-constructor clazz)
           keyvals))

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
