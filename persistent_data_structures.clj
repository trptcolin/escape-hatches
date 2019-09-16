(ns persistent-data-structures)

(defn- replace-array [f arr]
  (dotimes [i (alength arr)]
    (let [current (aget arr i)]
      (aset arr i (f current)))))

(defn- replace-node [f root]
  (let [node-arr (->> root .array)
        first-node (first node-arr)]
    (if (and first-node (instance? clojure.lang.PersistentVector$Node first-node))
      (doseq [node (filter identity node-arr)]
        (replace-node f node))
      (replace-array f node-arr))))

(defn mutate-vector [f xs]
  (replace-node f (.root xs))
  (replace-array f (.tail xs)))

(defn squares [xs]
  (mutate-vector (fn [_] (rand)) xs))

(defn -main [& args]
  (let [xs (vec (range 100))
        ys (squares xs)]
    (println xs)))

(-main)
