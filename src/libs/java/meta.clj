(ns libs.java.meta
  "Attach arbitrary metadata to Java objects"
  (:refer-clojure :exclude [meta type])
  (:require [clojure.core :as core])
  (:import (java.util WeakHashMap)))

(defn reset-meta-map []
  (def meta-map (WeakHashMap.)))

(reset-meta-map)

(defn meta [o]
  (.get meta-map o))

(defn set-meta! [o v]
  (.put meta-map o v))

(defn update-meta! [o f & args]
  (set-meta! o (apply f (meta o) args)))

(defn assoc-meta! [o & keyvals]
  (update-meta! o #(apply assoc % keyvals)))

(defn type [o]
  (or (when (or (keyword? o)
                (class? o))
        o)
      (:type (meta o))
      (core/type o)))
