(ns aoc-2025.day6
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [aoc.helpers :as h]))

(def sample-file "resources/2025/day6.sample.txt")
(def input-file "resources/2025/day6.txt")

(defn to-int-list [s]
  (mapv parse-long (str/split (str/trim s) #" +")))

(defn pt1 [ss]
  (let [operations (str/split (last ss) #" +")
        number-lines (mapv to-int-list (butlast ss))]

    (reduce + (map-indexed (fn [n op]
                             (cond
                               (= "+" op)
                               (reduce + 0 (map #(nth % n) number-lines))

                               (= "*" op)
                               (reduce * 1 (map #(nth % n) number-lines))))
                operations))))

(defn extract-numbers [ss col]
  ; returns [numbers, op]
  (let [operation (nth (last ss) col)
        number-col (map #(nth % col) (butlast ss))]
    [(parse-long (str/trim (apply str number-col))) operation]))

(defn parse-pt2 [ss]
  (for [col (range (count (first ss)))]
    (extract-numbers ss col)))

(defn pt2 [xs]
  (loop [x (parse-pt2 xs), accu 0M, op nil, grand-total 0]
    (prn :accu accu :op op :gt grand-total)
    (if (seq x)
      (let [[n new-op] (first x)]
        (cond
          (nil? n)
          (recur (rest x) 0 nil (+ grand-total accu))

          (not= new-op \space)
          (recur (rest x) n new-op grand-total)

          (= op \*)
          (recur (rest x) (* n accu) op grand-total)

          (= op \+)
          (recur (rest x) (+ n accu) op grand-total)

          :else
          (prn :SHOLD-NOT-HAPPEN :op op)

          ))
      (+ accu grand-total))))




(defn solve-1 
  ([] (solve-1 input-file))
  ([f] (pt1 (h/slurp-strings f))))

(defn solve-2 
  ([] (solve-2 input-file))
  ([f] (pt2 (h/slurp-strings f))))


(deftest tests []
  (are [x y] (= x y)
    4277556 (solve-1 sample-file)
    3263827 (pt2 (h/slurp-strings sample-file))))

(comment
  (solve-1)
  (solve-2))
