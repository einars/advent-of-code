(ns aoc-2025.day9
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [aoc.helpers :as h]))

(def sample-file "resources/2025/day9.sample.txt")
(def input-file "resources/2025/day9.txt")

(defn read-coordinates [f]
  (mapv h/to-int-list (h/slurp-strings f)))

(defn area [[x0 y0] [x1 y1]]
  (* (inc (abs (- x0 x1)))
    (inc (abs (- y0 y1)))))

(defn find-largest-area [coords]
  (first (reverse (sort (for [c0 coords
                              c1 coords]
                          (area c0 c1))))))

(defn pt1 [f]
  (find-largest-area (read-coordinates f)))




(defn solve-1 
  ([] (solve-1 input-file))
  ([f] (pt1 f)))

(defn solve-2 
  ([] (solve-2 input-file))
  ([f] (prn (h/slurp-strings f))))

(deftest tests []
  (are [x y] (= x y)
    99 (or 99 115)))

(comment
  (solve-1)
  (solve-2))
