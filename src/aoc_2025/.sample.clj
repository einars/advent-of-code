(ns aoc-2025.dayX
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [aoc.helpers :as h]))

(def sample-file "resources/2025/dayX.sample.txt")
(def input-file "resources/2025/dayX.txt")

(defn solve-1 
  ([] (solve-1 input-file))
  ([f] (prn (h/slurp-strings f))))

(defn solve-2 
  ([] (solve-2 input-file))
  ([f] (prn (h/slurp-strings f))))

(deftest tests []
  (are [x y] (= x y)
    99 (or 99 115)))

(comment
  (solve-1)
  (solve-2))
