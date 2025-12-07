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

(defn max-joltage-n [numbers digits]
  (loop [numbers numbers, remain (dec digits), accu []]
    (let [m0 (apply max (drop remain (reverse numbers))) 
          numbers (next (drop-while (complement #{m0}) numbers))]
      (if (= remain 0)
        (parse-long (apply str (conj accu m0)))
        (recur numbers (dec remain) (conj accu m0))))))


(defn explode-number-string [s]
  (map (comp parse-long str) s))



(defn solve-1 
  ([] (solve-1 input-file))
  ([f] (reduce + (map (comp max-joltage explode-number-string) (h/slurp-strings f)))))

(defn solve-2 
  ([] (solve-2 input-file))
  ([f] (reduce + (map #(max-joltage-n (explode-number-string %) 12) (h/slurp-strings f)))))

(deftest tests []
  (are [x y] (= x y)
    98 (max-joltage (explode-number-string "987654321111111"))
    98 (max-joltage-n (explode-number-string "987654321111111") 2)
    89 (max-joltage (explode-number-string "811111111111119"))
    78 (max-joltage (explode-number-string "234234234234278"))
    357 (solve-1 "resources/2025/day3.sample.txt")
    3121910778619 (solve-2 "resources/2025/day3.sample.txt")
    99 (or 99 115)))

(comment
  (solve-1)
  (solve-2))
