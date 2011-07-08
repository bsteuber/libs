(ns libs.app
  (:require [clojure.java.io :as io])
  (:use (libs io log)))

(defn app-name []
  (warn "application name not set")
  "my-app")

(defn app-dir []
  (doto (if-let [appdata (System/getenv "APPDATA")]
          (io/file appdata (app-name))
          (io/file (System/getProperty "user.home")
                   (str "." (app-name))))
    (.mkdir)))

(defn app-file [path]
  (->> path
       (map name)
       (reduce io/file (app-dir))))
