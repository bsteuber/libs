(ns lib.time
  (:require [clojure.string :as str]))

(defprotocol Timeable
  (to-ms [date]))

(defn joda-date [date]
  (org.joda.time.DateTime. (to-ms date)))

;; (defn sql-date [date]
;;   (java.sql.Date. (to-ms date)))

(defn sql-timestamp [date]
  (java.sql.Timestamp. (to-ms date)))

(defn java-date [date]
  (java.util.Date. (to-ms date)))

(defn date-str [date]
  (format "%1$tF %1$tR" (java-date date)))

(defn parse-date
  "Creates a joda DateTime object from a string like \"2010-1-31 20:15\".
   The time part is optional."
  [date-str]
  (let [make-time #(eval `(org.joda.time.DateTime.
                           ~@(take 7 (concat % (repeat 0)))))]
    (->> date-str
         (re-seq #"\d+")
         (map #(Integer. %))
         (make-time))))

(extend-protocol Timeable
  org.joda.time.DateTime
  (to-ms [date] (.getMillis date))
  java.sql.Date
  (to-ms [date] (.getTime date))
  java.sql.Timestamp
  (to-ms [date] (.getTime date))
  java.util.Date
  (to-ms [date] (.getTime date))
  String
  (to-ms [date] (to-ms (parse-date date)))
  Long
  (to-ms [date] date))

(defn now []
  (java.util.Date.))

(def seconds #(long (* 1000 %)))
(def minutes #(seconds (* 60 %)))
(def hours   #(minutes (* 60 %)))
(def days    #(hours   (* 24 %)))

(defn plus [date time]
  (+ (to-ms date)
     time))

(defn minus [& dates]
  (apply - (map to-ms dates)))

(defn before? [& dates]
  (apply < (map to-ms dates)))

(defn after? [& dates]
  (apply > (map to-ms dates)))