(ns libs.maps)

(defn invert-map [m]
  (into {} (map (fn [[x y]]
                  [y x])
                m)))

(defn patch-map [m1 m2]
  (merge-with #(if (and (map? %1)
                        (map? %2))
                 (patch-map %1 %2)
                 %2)
              m1
              m2))

(defn diff-map [m1 m2]
  (reduce (fn [m [key val]]
            (let [orig (m key)]
              (cond (= orig val)
                    (dissoc m key)
                    ;;
                    (and (map? orig)
                         (map? val))
                    (assoc m key (diff-map orig val))
                    ;;
                    :else
                    m)))
          m1
          m2))

(defn update-with [map updates]
  (reduce (fn [m [key update-fn]]
            (assoc m key (update-fn (key m))))
          map
          updates))
