(ns libs.ui
  (:use [libs.generic :only [def-frontend
                             conf]]))

(def-frontend :ui
  ask-user
  border
  button
  check-box
  combo-box
  dialog
  form
  grid
  horizontal
  icon
  label
  list-box
  message
  options
  panel
  password-field
  popup
  progress-bar
  rigid-area
  splitter
  tabs
  text-area
  text-field
  titled-border
  vertical
  window
  )

(defn close [o]
  (conf o :close true))
