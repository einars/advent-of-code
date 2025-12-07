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

(pt1 (h/slurp-strings sample-file))

(defn solve-1 
  ([] (solve-1 input-file))
  ([f] (pt1 (h/slurp-strings f))))

(defn solve-2
  ([] (slurp input-file)))

(deftest tests []
  (are [x y] (= x y)
    4277556 (solve-1 sample-file)))

(comment
  (solve-1)
  (solve-2))
