(ns libs.java.meta
  "Attach arbitrary metadata to Java objects"
  (:refer-clojure :exclude [meta type])
  (:require [clojure.core :as core])
  (:import (java.util WeakHashMap)))

(defn reset-meta-map []
  (def meta-map (WeakHashMap.)))

(reset-meta-map)

;; makes sure the meta-map is not modified concurrently
(def meta-thread-guard (agent nil))

(defn meta [o]
  (.get meta-map o))

(defn set-meta! [o v]
  (send meta-thread-guard
        (fn [_]
          (.put meta-map o v)))
  (await meta-thread-guard))

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
