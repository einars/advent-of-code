(ns aoc-2025.day7
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [aoc.helpers :as h]))

(def sample-file "resources/2025/day7.sample.txt")

(def input-file "resources/2025/day7.txt")

(defn make-beam [m]
  (let [s-pos (first (h/find-keys #{\S} m))]
    #{(:x s-pos)}))

(defn make-timeline-beam [m]
  (let [s-pos (first (h/find-keys #{\S} m))]
    {(:x s-pos) 1}))

(defn read-playground [f]
  (h/slurp-xy-map f))

(defn count-tachyon-splits 
  ([m {:keys [y]}] 
   (let [*n-splits (atom 0)]
     (loop [beam (make-beam m), y-pos 1]
       (if (= y-pos y)
         @*n-splits

         (let [new-beam (mapcat (fn [beam-pos]
                                  (if (= (get m {:y y-pos :x beam-pos}) \^)
                                    (do
                                      (swap! *n-splits inc)
                                      [(dec beam-pos) (inc beam-pos)])
                                    [beam-pos])) beam)
               new-beam (set new-beam)]
           (recur new-beam (inc y-pos))))))))

(defn count-timelines
  ([m {:keys [y]}] 
   (loop [beam (make-timeline-beam m), y-pos 1]
     (if (= y-pos y)
       beam

       (let [new-beam (reduce (fn [nb [x n-timelines]]

                                (if (= (get m {:y y-pos :x x}) \^)
                                  (-> nb
                                    (update (dec x) (fn [old-val] (+ n-timelines (or old-val 0))))
                                    (update (inc x) (fn [old-val] (+ n-timelines (or old-val 0)))))
                                  (-> nb
                                    (update x (fn [old-val] (+ n-timelines (or old-val 0)))))))
                        {} beam)]
         (recur new-beam (inc y-pos)))))))


(let [[m d] (read-playground sample-file)]
  (count-tachyon-splits m d))

(defn solve-1 
  ([] (let [[m d] (read-playground input-file)]
        (count-tachyon-splits m d))))

(defn solve-2
  ([] (slurp input-file)))

(deftest tests []
  (are [x y] (= x y)
    99 (or 99 115)))

(comment

  (solve-1)
  (solve-2))
