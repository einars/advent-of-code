(ns aoc-2025.day9
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.tools.trace :refer [deftrace]]
   [clojure.set :as set]
   [clojure.string :as str]
   [aoc.helpers :as h]))

(def sample-file "resources/2025/day9.sample.txt")
(def input-file "resources/2025/day9.txt")

(defn read-coordinates [f]
  (mapv h/to-int-list (h/slurp-strings f)))

(defn area [[x0 y0] [x1 y1]]
  (* (inc (abs (- x0 x1)))
    (inc (abs (- y0 y1)))))

(defn find-largest-area [coords]
  (first (reverse (sort (for [c0 coords
                              c1 coords]
                          (area c0 c1))))))


(defn limited-neighbors-4 [[x y] max-x max-y]
  (filter (fn [[x y]] (and (<= 0 x max-x) (<= 0 y max-y)))
    [[(inc x) y]
     [x (inc y)]
     [(dec x) y]
     [x (dec y)]]))


(defn floodfill-pack [p]
  (let [line-around (:line-around p)]
    (loop [front [[0 0]], filled #{}]
      (if-not (seq front)
        (do
          (prn :floodfill-done)
          (assoc p :void filled))

        (let [look-at (first front)
              new-filled (conj filled look-at)
              nbs (limited-neighbors-4 look-at (:max-x p) (:max-y p))
              nbs (filterv (complement line-around) nbs)
              new-front (filterv (complement filled) (concat nbs (rest front)))]

          (recur new-front new-filled))))))


(defn auto-range-incl [f t]
  (if (> f t)
    (range t (inc f))
    (range f (inc t))))


(defn fill-paths [p]
  (loop [paths (:pairs p), line-points #{}]

    (if-not (seq paths)
      (assoc p :line-around line-points)

      (let [[[x0 y0] [x1 y1]] (first paths)
            new-points (reduce conj
                         line-points
                         (for [x (auto-range-incl x0 x1)
                               y (auto-range-incl y0 y1)]
                           [x y]))]
        (recur (rest paths) new-points)))))


(defn build-pairs [coords]
  (let [f (first coords)
        l (last coords)]
    (conj (partition 2 1 coords) [l f])))


(defn packed-representation [coords]
  (let [xes (sort (set (map first coords)))
        yes (sort (set (map second coords)))
        remap-x (into {} (map-indexed (fn [n x] [ x (inc (* n 2)) ]) xes))
        remap-y (into {} (map-indexed (fn [n y] [ y (inc (* n 2)) ]) yes))
        new-coords (mapv (fn [[x y]] [(remap-x x) (remap-y y)]) coords)
        max-x (inc (apply max (vals remap-x)))
        max-y (inc (apply max (vals remap-y)))
        _ (prn :packed-to max-x :times max-y)]
    {:max-x max-x
     :max-y max-y
     :rx remap-x
     :ry remap-y
     :coords new-coords
     :pairs (build-pairs new-coords) }))


(defn valid-square? [[x0 y0] [x1 y1] p]
  ; if none of the square lines touch the void
  (not (or 
         (some (:void p) (for [x (auto-range-incl x0 x1)] [x y0]))
         (some (:void p) (for [x (auto-range-incl x0 x1)] [x y1]))
         (some (:void p) (for [y (auto-range-incl y0 y1)] [x0 y]))
         (some (:void p) (for [y (auto-range-incl y0 y1)] [x1 y])))))

(defn unpacked-area [c0 c1 p]
  (let [[x0 y0] c0
        [x1 y1] c1
        rev-x (set/map-invert (:rx p))
        rev-y (set/map-invert (:ry p))]

    (area [(rev-x x0) (rev-y y0)] [(rev-x x1) (rev-y y1)])))

(defn find-largest-area-pt2 [p]
  (first (reverse (sort-by :area (for [c0 (:coords p)
                                       c1 (:coords p)
                                       :when (valid-square? c0 c1 p)]
                                   {:c0 c0 :c1 c1 :area (unpacked-area c0 c1 p) :a (area c0 c1)})))))



(defn repr-p [p]
  (apply str (for [y (range 0 (inc (:max-y p)))]
               (str (apply str (for [x (range 0 (inc (:max-x p)))]
                                 (cond 
                                   ((:line-around p) [x y]) "#"
                                   ((:void p) [x y]) "."
                                   :else " "))) "\n"))))


(defn pt2 [f]
  (let [p (floodfill-pack (fill-paths (packed-representation (read-coordinates f))))]
    (prn :mx (:max-x p) :my (:max-y p))
    ;(spit "/home/e/temp/repr.txt" (repr-p p))
    ;(print (repr-p p))
    (find-largest-area-pt2 p)))

;(pt2 sample-file)
;(pt2 input-file)


(defn pt1 [f]
  (find-largest-area (read-coordinates f)))


(defn solve-1 
  ([] (solve-1 input-file))
  ([f] (pt1 f)))

(defn solve-2 
  ([] (solve-2 input-file))
  ([f] (pt2 f)))

(deftest tests []
  (are [x y] (= x y)
    99 (or 99 115)))

(comment
  (solve-1)
  (solve-2))
