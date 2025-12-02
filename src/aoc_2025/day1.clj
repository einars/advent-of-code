(ns aoc-2025.day1
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [aoc.helpers :as h]))

(def sample-directions ["L68" "L30" "R48" "L5" "R60" "L55" "L1" "L99" "R14" "L82"])

(defn count-zeros [xs] (count (filter #{0} xs)))

(defn move-dial [cur-n dir-amount]
  (let [dir (first dir-amount)
        n (parse-long (apply str (drop 1 dir-amount)))]
    (if (= dir \L)
      (mod (- cur-n n) 100)
      (mod (+ cur-n n) 100))))

(defn pt2 [cur-n dir-amounts]
  (loop [cur-n cur-n, zeros 0, dir-amounts dir-amounts]
    (if (seq dir-amounts)
      (let [da (first dir-amounts)
            dir (first da)
            n (parse-long (apply str (drop 1 da)))

            ; aargh
            zeros ( + zeros (quot n 100))
            n (mod n 100)

            np-raw (if (= dir \L) (- cur-n n) (+ cur-n n))
            np (mod np-raw 100)

            moved-zero? (if (= dir \L)
                          (and (>= n cur-n) (not= cur-n 0))
                          (> n (- 99 cur-n)))]
        (recur np (if moved-zero? (inc zeros) zeros) (rest dir-amounts)))
      zeros)))

(defn click-pt1 [start directions]
  (reductions move-dial start directions))

(defn solve-1 
  ([] (count-zeros (click-pt1 50 (h/slurp-strings "resources/2025/day1.txt")))))

(defn solve-2
  ([] (pt2 50 (h/slurp-strings "resources/2025/day1.txt"))))

;(defn solve-2 []
;  (top3-elves-carrying-the-most (h/slurp-strings "resources/2022/day1.txt")))

(deftest tests []
  (are [x y] (= x y)
    3 (count-zeros (click-pt1 50 sample-directions))
    6 (pt2 50 sample-directions)))

(comment
  (solve-1)
  (solve-2))
