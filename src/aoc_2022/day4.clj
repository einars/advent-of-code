(ns aoc-2022.day4
  (:require [clojure.test :as test :refer [deftest is]])
  (:require [clojure.string :as str])
  (:require [aoc-2022.helpers :as h]))

(defn make-range [range-s]
  (map #(Integer/parseInt %) (str/split range-s #"-")))

(defn convert-to-ranges
  [line]
  (map make-range (str/split line #",")))

(defn range-fully-contains
  [[f1 t1] [f2 t2]]
  (and (>= f2 f1 t2)
       (>= f2 t1 t2)
    )
  )

(defn ranges-fully-contain-any?
  [[f1 t1] [f2 t2]]
  (or (<= f1 f2 t2 t1)
      (<= f2 f1 t1 t2)))

(defn solve-1
  ([] (solve-1 "resources/day4.txt"))
  ([file]
    (->>
      (h/slurp-strings file)
      (map convert-to-ranges)
      (filter #(ranges-fully-contain-any? (first %) (second %)))
      count)))

(comment defn solve-2
  ([] (solve-2 "resources/day4.txt"))
  ([file]
    (->>
      (h/slurp-strings file)
      ; ...
      )))


(deftest test [] 
  (test/are [x y] (= x y)
    false (ranges-fully-contain-any? '(2 4) '(6 8))
    true (ranges-fully-contain-any? '(6 6) '(4 6))
    true (ranges-fully-contain-any? '(4 6) '(6 6))
    2 (solve-1 "resources/day4.test.txt")
    ; 0 (solve-2 "resources/day4.test.txt")
    ))

(test/run-tests)
