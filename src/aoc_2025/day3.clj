(ns aoc-2025.day3
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [aoc.helpers :as h]))

(def input-file "resources/2025/day3.txt")

(defn max-joltage [numbers]
  (let [m0 (apply max (drop 1 (reverse numbers)))
        numbers (drop-while (complement #{m0}) numbers)
        m1 (apply max (drop 1 numbers))]
    (+ m1 (* m0 10))))

(defn explode-number-string [s]
  (map (comp parse-long str) s))



(defn solve-1 
  ([] (solve-1 input-file))
  ([f] (reduce + (map (comp max-joltage explode-number-string) (h/slurp-strings f)))))

(defn solve-2
  ([] (slurp input-file)))

(deftest tests []
  (are [x y] (= x y)
    98 (max-joltage (explode-number-string "987654321111111"))
    89 (max-joltage (explode-number-string "811111111111119"))
    78 (max-joltage (explode-number-string "234234234234278"))
    357 (solve-1 "resources/2025/day3.sample.txt")
    99 (or 99 115)))

(comment
  (solve-1)
  (solve-2))
