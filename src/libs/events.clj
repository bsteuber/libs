(ns libs.events
  (:require [libs.java.meta :as m])
  (:use (libs log)))

(defmulti handle (fn [receiver key & args]
                   [(m/type receiver) key]))

(defmacro defhandler [receiver key args & rest]
  `(defmethod handle [~receiver ~key] [_# _# ~@args] ~@rest))

(defmethod handle :default
  [receiver key & _]
  (debug "unhandled event" (m/type receiver) key))
