(ns libs.gui.core
  (:use (libs debug)
        [libs.generic :only [as]]))

(def gui-impl nil)

(defn set-impl [impl]
  (def gui-impl impl))

(defmulti impl-class (fn [impl widget]
                          [impl widget]))

(defmethod impl-class :default
  [b w]
  (fail "No implementation for widget" w "in backend" b))

(defmacro def-impl-classes [backend & args]
  `(do ~@(for [[name class] (partition 2 args)]
           `(defmethod impl-class [~backend ~(keyword name)]
              [_# _#]
              ~class))))

(defmacro def-impl-specific [& names]
  `(do ~@(for [name names]
           `(defn ~name [& args#]
              (as (impl-class gui-impl ~(keyword name))
                  args#)))))

(def-impl-specific
  window
  dialog
  panel
  border
  titled-border
  horizontal
  vertical
  splitter
  grid
  tabs
  form
  rigid-area
  label
  button
  text-field
  text-area
  password-field
  check-box
  list-box
  combo-box
  options
  icon
  progress-bar)