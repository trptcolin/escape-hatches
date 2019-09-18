(ns persistent-data-structures
  (:require [clojure.test :as test]))

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

(defn do-something [xs]
  (mutate-vector (constantly :boom) xs))

(test/deftest test-immutability
  (test/testing "the awesomeness of immutability"
    (let [xs [0 1 2 3 4 5 6 7 8 9]
          ys (map (fn [x] (* x x)) xs)]
      (test/is (= (range 10) xs))))

  (test/testing "the pleasing nature of... wait what??"
    (let [xs [0 1 2 3 4 5 6 7 8 9]]
      (do-something xs)
      (test/is (= (repeat 10 :boom) xs)))))

(test/run-tests *ns*)

;; clj persistent-data-structures.clj
