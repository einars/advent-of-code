(ns aoc-2025.day8
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.math :as math]
   [aoc.helpers :as h]))

(def sample-file "resources/2025/day8.sample.txt")
(def input-file "resources/2025/day8.txt")

(defn create-box [s]
  (mapv parse-long (str/split s #",")))

(defn read-problem [f]
  (mapv create-box (h/slurp-strings f)))

(defn create-circuits [boxes]
  (mapv #(set [%]) boxes))

(defn square [n] (* n n))

(defn box-dist [[a0 a1 a2] [b0 b1 b2]]
  (math/sqrt (+ (square (abs (- a0 b0)))
               (square (abs (- a1 b1)))
               (square (abs (- a2 b2))))))

(defn dist-pairs [boxes]
  (set (filter some? (for [a boxes
                           b boxes]
                       (when (not= a b)
                         {:boxes (set [a b]) :dist (box-dist a b)})))))

(defn join-boxes [boxes circs]
  (let [[b0 b1] (vec boxes)
        c0 (first (filter (fn [c] (get c b0)) circs))
        c1 (first (filter (fn [c] (get c b1)) circs))
        without-b (filter (fn [c] (and (not= c c0) (not= c c1))) circs)]
    (if (= c0 c1)
      circs ; no join needed
      (conj without-b (set/union c0 c1)))))

(defn pt1-score [xs]
  (reduce * (take 3 (reverse (sort (map count xs))))))

(defn pt1 [f n]
  (let [boxes (read-problem f)
        dists (take n (sort-by :dist (dist-pairs boxes)))
        circs (create-circuits boxes)]
    
    (pt1-score (reduce (fn [circs {:keys [boxes]}]
                         (join-boxes boxes circs)
                         ) circs dists))))

(defn pt2-score [box-pair]
  (let [[box1 box2] (vec box-pair)]
    (* (first box1) (first box2))))

(defn pt2 [f]
  (let [boxes (read-problem f)
        box-pairs (map :boxes (sort-by :dist (dist-pairs boxes)))]

    (loop [box-pairs box-pairs, circs (create-circuits boxes)]
      (let [box-pair (first box-pairs)
            new-circs (join-boxes box-pair circs)]
        (if (= 1 (count new-circs))
          (pt2-score box-pair)
          (recur (rest box-pairs) new-circs))))))

(defn solve-1 
  ([] (solve-1 input-file))
  ([f] (pt1 f 1000)))

(defn solve-2 
  ([] (solve-2 input-file))
  ([f] (pt2 f)))

(deftest tests []
  (are [x y] (= x y)
    40 (pt1 sample-file 10)
    25272 (pt2 sample-file)))

(comment
  (solve-1)
  (solve-2))
