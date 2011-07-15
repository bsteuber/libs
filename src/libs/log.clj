(ns libs.log
  (:require [clojure.string :as str]))

(def log-levels [:error :warning :info :debug])

(def log-level-index (zipmap log-levels (range (count log-levels))))

(def ^:dynamic current-log-index)

(defmacro with-log-level [level & body]
  `(binding [current-log-index (log-level-index ~level)]
     ~@body))

(defn set-log-level [level]
  (def current-log-index (log-level-index level)))

(set-log-level :debug)

(def log-to-stdout? true)

(defn set-log-stdout [x]
  (def log-to-stdout x))

(def ^:dynamic log-file nil)

(defn set-log-file [f]
  (def log-file f))

(defmacro with-log-file [f & body]
  `(binding [log-file f]
     ~@body))

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
  (apply log :warning args))

(defn info [& args]
  (apply log :info args))

(defn debug [& args]
  (apply log :debug args))

(defn error [& args]
  (apply log :error args))
