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


(pt1 (h/slurp-blocks sample-file))



(defn solve-1 
  ([] (solve-1 input-file))
  ([f] (pt1 (h/slurp-blocks f))))

(defn solve-2 
  ([] (solve-2 input-file))
  ([f] (prn (h/slurp-strings f))))

(deftest tests []
  (are [x y] (= x y)
    99 (or 99 115)))

(comment
  (solve-1)
  (solve-2))
