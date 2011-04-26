(ns libs.seq
  (:use (libs debug)))

(defn one-element? [seq]
  (and (first seq)
       (not (next seq))))

(defn one-element
  ([seq] (one-element seq "Sequence"))
  ([seq descr]
     (if (one-element? seq)
       (first seq)
       (fail descr "should contain exactly one element!\n" (print-str seq)))))