(ns libs.db
  (:use (libs app io maps)))

(defn app-db
  ([path] (app-db path {}))
  ([path defaults]
     (agent defaults
            :meta {:db       :file
                   :file     (app-file path)
                   :defaults defaults})))

(defn get* [db & path]
  (get-in @db path))

(defn set* [db & args]
  (let [value (last args)
        path  (butlast args)]
    (send db assoc-in path value)))

(def db-type (comp :db meta))

(defmulti read-db  db-type)
(defmulti write-db db-type)

(defn clear-db [db]
  (send db (fn [_] (:defaults (meta db)))))

(defmethod read-db :file [db]
  (let [{:keys [file defaults]} (meta db)]
    (send db
          (constantly
           (patch-map
            defaults
            (deserialize file))))))

(defmethod write-db :file [db]
  (let [{:keys [file defaults]} (meta db)]
    (serialize file
               (diff-map @db
                         defaults))))
