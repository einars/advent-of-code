(ns aoc-2025.day2
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [aoc.helpers :as h]))

(def test-ranges "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(defn half-base [n]
  (cond
    (< n 10) nil
    (< n 100) 10
    (< n 1000) nil
    (< n 10000) 100
    (< n 100000) nil
    (< n 1000000) 1000
    (< n 10000000) nil
    (< n 100000000) 10000
    (< n 1000000000) nil
    (< n 10000000000) 100000
    (< n 100000000000) nil
    (< n 1000000000000) 1000000))

(defn is-invalid-id [n]
  (when-let [hb (half-base n)]
    (let [a (quot n hb)
          b (mod n hb)]
      (= a b))))

(defn is-invalid-pt2 
  ([n] (is-invalid-pt2 (str n) 1))
  ([s frag-len] 
   (let [times (quot (count s) frag-len)
         frag (subs s 0 frag-len)
         test (str/join (repeat times frag))]
     (if (< times 2)
       false
       (if (= test s)
         true
         (recur s (inc frag-len)))))))




(defn invalid-in-range [from to]
  (filter is-invalid-id (range from (inc to))))

(defn invalid-in-range-pt2 [from to]
  (filter is-invalid-pt2 (range from (inc to))))

(defn make-range [s]
  (let [[a b] (str/split s #"-")]
    [(parse-long a) (parse-long b)]))

(defn parse-ranges [s]
  (let [text-ranges (str/split (str/trim s) #",")]
    (map make-range text-ranges)))


(defn read-ranges [f]
  (parse-ranges (slurp f)))

(defn solve-1 
  ([] (reduce + 0 (mapcat (fn [[a b]] (invalid-in-range a b)) (read-ranges "resources/2025/day2.txt")))))

(defn solve-2
  ([] (reduce + 0 (mapcat (fn [[a b]] (invalid-in-range-pt2 a b)) (read-ranges "resources/2025/day2.txt")))))

(deftest tests []
  (are [x y] (= x y)
    1227775554 (reduce + 0 (mapcat (fn [[a b]] (invalid-in-range a b)) (parse-ranges test-ranges)))
    4174379265 (reduce + 0 (mapcat (fn [[a b]] (invalid-in-range-pt2 a b)) (parse-ranges test-ranges)))
    1 (count (invalid-in-range 1188511880 1188511890))
    2 (count (invalid-in-range 11 22))
    true (is-invalid-pt2 212121)
    false (is-invalid-pt2 12)
    '(11 22) (invalid-in-range-pt2 11 22)
    '(99 111) (invalid-in-range-pt2 99 115)))

(comment
  (solve-1)
  (solve-2))
