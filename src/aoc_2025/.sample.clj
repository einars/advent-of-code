(ns aoc-2025.dayX
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [aoc.helpers :as h]))

(def input-file "resources/2025/dayX.txt")

(defn solve-1 
  ([] (slurp input-file)))

(defn solve-2
  ([] (slurp input-file)))

(deftest tests []
  (are [x y] (= x y)
    99 (or 99 115)))

(comment
  (solve-1)
  (solve-2))
