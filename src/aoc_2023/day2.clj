(ns aoc-2023.day2
  (:require
    [clojure.test :as test :refer [deftest are]]
    [instaparse.core :as insta]
    [aoc.helpers :as h]))

(def ^:dynamic *initial-bag* {:red 12
                              :green 13
                              :blue 14 })


(def sample-games ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
                   "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
                   "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
                   "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
                   "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green" ])

(def game-parser (insta/parser
                   "
    <game> = <'Game '> int <': '> reveals
    <reveals> = reveal (<'; '> reveal)*
    reveal = color (<', '> color)*
    color = int <' '> ('red' | 'blue' | 'green')
    <int> = #'\\d+'
    "))

; (game-parser "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
;("1" 
;  [:reveal [:color "3" "blue"] [:color "4" "red"]]
;  [:reveal [:color "1" "red"] [:color "2" "green"] [:color "6" "blue"]]
;  [:reveal [:color "2" "green"]])


(defn make-reveal [colors] 
  (into {} (mapv (fn [[_ n k]] [(keyword k) (parse-long n)]) (rest colors))))

(defn parse-game [g]
  (let [[game & reveals] (game-parser g)]
    {:game (parse-long game)
     :reveals (mapv make-reveal reveals)}))


(defn reveal-possible? [reveal]
  (every? (fn [[color count]] (>= (color *initial-bag*) count)) reveal))

(defn game-possible? [game]
  (every? reveal-possible? (:reveals game)))

(defn get-minimum-score [{:keys [reveals]}]
  (*
    (apply max (filter some? (map :red reveals)))
    (apply max (filter some? (map :blue reveals)))
    (apply max (filter some? (map :green reveals)))))


(defn solve-1 
  ([] (solve-1 (h/slurp-strings "resources/2023/day2.txt")))
  ([lines]
   (->> lines
     (mapv parse-game)
     (filter game-possible?)
     (mapv :game)
     (reduce +))))


(defn solve-2
  ([] (solve-2 (h/slurp-strings "resources/2023/day2.txt")))
  ([lines]
   (->> lines
     (mapv parse-game)
     (mapv get-minimum-score)
     (reduce +))))

(deftest tests []
  (are [x y] (= x y)
    {:game 1, :reveals [{:blue 3 :red 4}
                        {:red 1 :green 2 :blue 6}
                        {:green 2}]}
    (parse-game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
    8 (solve-1 sample-games)
    2286 (solve-2 sample-games)))

(comment
  (solve-1)
  (solve-2))

