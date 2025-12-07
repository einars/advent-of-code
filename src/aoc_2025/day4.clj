(ns aoc-2025.day4
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [aoc.helpers :as h]))

(def sample-file "resources/2025/day4.sample.txt")
(def input-file "resources/2025/day4.txt")

(defn pt1 [m]
  (count (filter #(< % 4) (for [k (keys m)]
                            (count (filter #{\@} (map m (h/neighbors-8 k))))))))

(defn to-remove [m]
  (filter
    (fn [k]
      (< (count (filter #{\@} (map m (h/neighbors-8 k)))) 4))
    (keys m)))

(defn remove-rolls [m rolls]
  (reduce dissoc m rolls))

(defn pt2 [m]
  (loop [n-removed 0, m m]
    (let [available (to-remove m)]
      (if (empty? available)
        n-removed
        (recur (+ n-removed (count available)) (remove-rolls m available))))))

(defn solve-1 
  ([] (solve-1 input-file))
  ([f] (pt1 (h/slurp-map f))))

(defn solve-2 
  ([] (solve-2 input-file))
  ([f] (pt2 (h/slurp-map f))))

(deftest tests []
  (are [x y] (= x y)
    13 (solve-1 sample-file)))

(comment
  (solve-1)
  (solve-2))
