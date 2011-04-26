(ns libs.log
  (:require [clojure.string :as str]))

(def log-levels [:error :warn :info :debug])

(def log-level-index (zipmap log-levels (range (count log-levels))))

(def current-log-index)

(defn set-log-level [level]
  (def current-log-index (log-level-index level)))

(set-log-level :debug)

(def log-to-stdout? true)

(defn set-log-stdout [x]
  (def log-to-stdout x))

(def log-file nil)

(defn set-log-file [f]
  (def log-file f))

(def date-formatter (java.text.SimpleDateFormat. "yyyy-MM-dd kk:mm"))

(defn log [level & args]
  (when (<= (log-level-index level)
            current-log-index)
    (let [time (str "[" (.format date-formatter (java.util.Date.)) "]")
          text (apply println-str (name level) time args)]
      (when log-to-stdout?
        (print text)
        (flush))
      (when log-file
        (spit log-file text :append true)))))

(defn warn [& args]
  (apply log :warn args))

(defn info [& args]
  (apply log :info args))

(defn debug [& args]
  (apply log :debug args))

(defn error [& args]
  (apply log :error args))
