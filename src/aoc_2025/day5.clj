(ns aoc-2025.day5
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.tools.trace :refer [trace deftrace]]
   [clojure.string :as str]
   [aoc.helpers :as h]))

(def sample-file "resources/2025/day5.sample.txt")
(def input-file "resources/2025/day5.txt")

(defn make-range [s]
  (let [[a b] (str/split s #"-")]
    [(parse-long a) (parse-long b)]))

(defn range-match [n [lo hi]]
  (<= lo n hi))

(defn is-fresh? [n ranges]
  (some #(range-match n %) ranges))

(defn pt1 [[ranges numbers]]
  (let [ranges (mapv make-range ranges)
        numbers (mapv parse-long numbers)]
    (count (filter #(is-fresh? % ranges) numbers))))

(defn overlaps? [[lo1 hi1] [lo2 hi2]]
  (or (<= lo2 lo1 hi2)
    (<= lo2 hi1 hi2)
    (<= lo1 lo2 hi1) 
    (<= lo1 hi2 hi1)))

(defn join-ranges [[lo1 hi1] [lo2 hi2]]
  [(min lo1 lo2) (max hi1 hi2)])

(defn pack-maximum [r pool]
  (if-let [maybe-overlapping (first (filter #(overlaps? r %) pool))]
    (recur (join-ranges r maybe-overlapping) (filter #(not= % maybe-overlapping) pool))
    [r pool]))

(defn range-width [[lo hi]]
  (inc (- hi lo)))

(defn calc-range-width [ranges]
  (loop [r (first ranges), pool (rest ranges), accu 0]
    ;(prn :packing r :pool pool)
    (let [[r pool] (pack-maximum r pool)]
      ;(prn :packed-to r :new-pool pool :rw (range-width r))
      (if (seq pool)
        (recur (first pool) (rest pool) (+ accu (range-width r)))
        (+ accu (range-width r))))))


(defn pt2 [[ranges _]]
  (let [ranges (mapv make-range ranges)]
    (calc-range-width ranges)))

(defn solve-1 
  ([] (solve-1 input-file))
  ([f] (pt1 (h/slurp-blocks f))))

(defn solve-2 
  ([] (solve-2 input-file))
  ([f] (pt2 (h/slurp-blocks f))))

(deftest tests []
  (are [x y] (= x y)
    99 (or 99 115)))

(comment
  (solve-1)
  (solve-2))
