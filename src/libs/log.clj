(ns libs.log
  (:require [clojure.string :as str])
  (:use (libs args)))

(def log-levels [:error :warning :info :debug])

(def log-level-index
  (zipmap log-levels
          (range (count log-levels))))

(def ^:dynamic *current-log-index*)

(defn set-log-level [level]
  (def *current-log-index* (log-level-index level)))

(defmacro with-log-level [level & body]
  `(binding [*current-log-index* (log-level-index ~level)]
     ~@body))

(set-log-level :debug)

(def ^:dynamic *log-writer* *out*)

(defn log-to [f]
  (def *log-writer* f))

(defmacro with-logging-to [f & body]
  `(binding [*log-writer* ~f]
     ~@body))

(def date-formatter (java.text.SimpleDateFormat. "yyyy-MM-dd kk:mm"))

(defn log [level & args]
  (when (<= (log-level-index level)
            *current-log-index*)
    (let [time (str "[" (.format date-formatter (java.util.Date.)) "]")
          [option-map args] (parse-leading-options [:log-to] args)
          text (apply println-str (name level) time args)
          log-writer (option-map :log-to *log-writer*)]
      (cond (instance? java.io.Writer log-writer)
            (binding [*out* log-writer]
              (print text)
              (flush))
            ;;
            (or (string? log-writer)
                (instance? java.io.File log-writer))
            (spit log-writer text :append true)
            ;; maybe add support for multiple writers as seqs
            ))))

(defn warn [& args]
  (apply log :warning args))

(defn info [& args]
  (apply log :info args))

(defn debug [& args]
  (apply log :debug args))

(defn error [& args]
  (apply log :error args))
