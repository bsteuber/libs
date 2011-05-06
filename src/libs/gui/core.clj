(ns libs.gui.core
  (libs debug))

(def gui-impl nil)

(defn set-impl [impl]
  (def gui-impl impl))

(defmulti impl-class (fn [impl widget]
                          [impl widget]))

(defmethod impl-class :default
  [b w]
  (fail "No implementation for widget" w "in backend" b))

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