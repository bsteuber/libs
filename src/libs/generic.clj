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

(defmulti put (fn [o val]
                [(m/type o)
                 (m/type val)]))

(defmulti config (fn [o  _]
                   (m/type o)))

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

(defmethod set [Object :args]
  [o _ args]
  (doseq [arg args]
    (put o arg)))

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

(defn set-all [o args]
  (let [args (if (map? args)
               args
               (partition 2 (parse-args args)))]
    (doseq [[k v] args]
      (try (set o k v)
           (catch Exception e
             (error "Error in generic setter:"
                    "\n  slot:       " k
                    "\n  class:      " (class o)
                    "\n  value:      " v
                    "\n  value class:" (class v))
             (throw e)))))
  o)

(defmethod config :default [o args]
  (set-all o args))

(defn conf [o & args]
  (config o args))

(defn make [clazz & args]
  (config (call-constructor clazz)
          args))

(defmethod as :default
  [clazz args]
  (config (call-constructor clazz)
          args))