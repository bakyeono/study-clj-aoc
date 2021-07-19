(ns util.util)

(defn get-first-duplicated-by [k coll]
  (let [reduction (reduce (fn [seen? value]
                            (if (seen? (k value))
                              (reduced value)
                              (conj seen? (k value))))
                          #{}
                          coll)]
    (if (set? reduction)
      nil
      reduction)))
(def get-first-duplicated (partial get-first-duplicated-by identity))
