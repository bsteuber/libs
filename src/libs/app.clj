(ns libs.app
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:use (libs debug io log)))

(def app-name nil)

;; TODO: IDE neutral way?
(defn development-mode?
  "Checks whether swank is on the classpath and thus we're developing"
  []
  (->> "java.class.path"
       System/getProperty
       (re-find #"swank")
       boolean))

(def production-mode?
  (complement development-mode?))

(defn app-dir []
  (if (development-mode?)
    (make-dir "app-dir")
    (if app-name
      (if-let [appdata (System/getenv "APPDATA")]
        ;; windows
        (make-dir appdata app-name)
        ;; linux, mac
        (make-dir (System/getProperty "user.home")
                  (str "." app-name)))
      (fail "app-name must be set before calling app-dir"))))

(defn app-file [path]
  (make-file (app-dir) path))
