(ns libs.ui
  (:use [libs.generic :only [def-frontend]]))

(def-frontend :ui
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
  progress-bar
  ask-user
  message)