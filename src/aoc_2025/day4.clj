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



(pt1 (h/slurp-map sample-file))

(defn solve-1 
  ([] (solve-1 input-file))
  ([f] (pt1 (h/slurp-map f))))

(defn solve-2 
  ([] (solve-2 input-file))
  ([f] (prn (h/slurp-strings f))))

(deftest tests []
  (are [x y] (= x y)
    13 (solve-1 sample-file)))

(comment
  (solve-1)
  (solve-2))
