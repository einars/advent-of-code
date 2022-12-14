(ns aoc-2022.day22
  (:require
    [clojure.test :as test :refer [deftest]]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.trace :refer [trace deftrace]]
    [clojure.pprint :as pp]
    [aoc.helpers :as h]))

(def turn {:E {:R :S, :L :N}
           :S {:R :W, :L :E}
           :W {:R :N, :L :S}
           :N {:R :E, :L :W}})

(def ^:dynamic *drawing* {})
(defn wrap-pt1
  [state]
  (let [x (get-in state [:at :x])
        y (get-in state [:at :y])]
    (condp = (:facing state)
      :N (assoc-in state [:at :y] (->> (keys *drawing*)
                                    (filter #(= (:x %) x))
                                    (map :y)
                                    (apply max)))
      :S (assoc-in state [:at :y] (->> (keys *drawing*)
                                    (filter #(= (:x %) x))
                                    (map :y)
                                    (apply min)))
      :E (assoc-in state [:at :x] (->> (keys *drawing*)
                                    (filter #(= (:y %) y))
                                    (map :x)
                                    (apply min)))
      :W (assoc-in state [:at :x] (->> (keys *drawing*)
                                    (filter #(= (:y %) y))
                                    (map :x)
                                    (apply max))))))

(defn current-face 
  [{:keys [csize at]}]
  [(+ (quot (at :x) csize) (* (quot (at :y) csize) 4))
   (mod (at :x) csize)
   (mod (at :y) csize)
   (- csize 1 (mod (at :x) csize))
   (- csize 1 (mod (at :y) csize))])

(defn at-face 
  "move x, y (0..49) to the specified region n
   0  [1] [2]  3 
   4  [5]  6   7
  [8] [9] 10  11
 [12] 13  14  15"
  [csize n {:keys [x y]}]
  {:x (+ x (* csize (mod n 4)))
   :y (+ y (* csize (quot n 4)))})

(defn wrap-pt2-test
  "Manual wrapper-fn for test case"
  [{:keys [csize] :as state}]
  (let [[face rx ry invx invy] (current-face state)]
    (condp = [face (:facing state)]
      [2 :N] (-> state
               (assoc :facing :S)
               (assoc :at (at-face csize 2 {:x invx, :y 0})))
      [4 :N] (-> state
               (assoc :facing :S)
               (assoc :at (at-face csize 4 {:x invx :y 0})))
      [5 :N] (-> state
               (assoc :facing :E)
               (assoc :at (at-face csize 2 {:x 0 :y rx})))
      [11 :N] (-> state
                (assoc :facing :W)
                (assoc :at (at-face csize 6 {:x invx :y 0})))
      [4 :S] (-> state
               (assoc :facing :N)
               (assoc :at (at-face csize 10 {:x invx, :y ry})))
      [5 :S] (-> state
               (assoc :facing :E)
               (assoc :at (at-face csize 10 {:x 0 :y invx})))
      [10 :S] (-> state
                (assoc :facing :N)
                (assoc :at (at-face csize 4 {:x invx :y ry})))
      [11 :S] (-> state
                (assoc :facing :E)
                (assoc :at (at-face csize 6 {:x 0 :y invx})))
      [2 :W] (-> state
               (assoc :facing :N)
               (assoc :at (at-face csize 5 {:x ry, :y 0})))
      [4 :W] (-> state
               (assoc :facing :N)
               (assoc :at (at-face csize 11 {:x invy :y invx})))
      [10 :W] (-> state
                (assoc :facing :N)
                (assoc :at (at-face csize 5 {:x invy :y invx})))
      [2 :E] (-> state
               (assoc :facing :W)
               (assoc :at (at-face csize 11 {:x invx, :y ry})))
      [6 :E] (-> state
               (assoc :facing :S)
               (assoc :at (at-face csize 11 {:x invy :y 0})))
      [11 :E] (-> state
                (assoc :facing :W)
                (assoc :at (at-face csize 2 {:x invx :y ry}))))))

(defn wrap-pt2-input
  "Manual wrapper-fn for my input. No, I don't want to make a generic cube layouter."
  [{:keys [csize] :as state}]
  (let [[face rx ry _invx invy] (current-face state)
        bottom (dec csize)
        right (dec csize)]
    (condp = [face (:facing state)]
      [1 :N] (-> state
               (assoc :facing :E)
               (assoc :at (at-face csize 12 {:x 0, :y rx})))
      [1 :W] (-> state
               (assoc :facing :E)
               (assoc :at (at-face csize 8 {:x 0, :y invy})))
      [2 :N] (-> state
               (assoc :facing :N)
               (assoc :at (at-face csize 12 {:x rx :y bottom})))
      [2 :E] (-> state
               (assoc :facing :W)
               (assoc :at (at-face csize 9 {:x right :y invy})))
      [2 :S] (-> state
               (assoc :facing :W)
               (assoc :at (at-face csize 5 {:x right :y rx})))
      [5 :W] (-> state
               (assoc :facing :S)
               (assoc :at (at-face csize 8 {:x ry :y 0})))
      [5 :E] (-> state
               (assoc :facing :N)
               (assoc :at (at-face csize 2 {:x ry :y bottom})))
      [8 :N] (-> state
               (assoc :facing :E)
               (assoc :at (at-face csize 5 {:x 0 :y rx})))
      [8 :W] (-> state
               (assoc :facing :E)
               (assoc :at (at-face csize 1 {:x 0 :y invy})))
      [9 :E] (-> state
               (assoc :facing :W)
               (assoc :at (at-face csize 2 {:x right :y invy})))
      [9 :S] (-> state
               (assoc :facing :W)
               (assoc :at (at-face csize 12 {:x right :y rx})))
      [12 :W] (-> state
                (assoc :facing :S)
                (assoc :at (at-face csize 1 {:x ry :y 0})))
      [12 :S] (-> state
                (assoc :facing :S)
                (assoc :at (at-face csize 2 {:x rx :y 0})))
      [12 :E] (-> state
                (assoc :facing :N)
                (assoc :at (at-face csize 9 {:x ry :y bottom}))))))


(def ^:dynamic *wrapper* wrap-pt1)

(defn tile-facing
  "Step into :facing direction, wrapping regions as necessary. 
  Returns tile at new pos (possibly wall) and updated state."
  [state test-at]
  (if-let [tile (*drawing* test-at)]
    [tile (assoc state :at test-at)]
    (let [new-state (*wrapper* state)]
      [(*drawing* (:at new-state)) new-state])))

(defn take-step 
  "Step into :facing direction, wrapping regions as necessary. nil if unable to proceed."
  [{:keys [facing at] :as state}]
  (let [[tile new-state] (condp = facing
                           :N (tile-facing state (update at :y dec))
                           :S (tile-facing state (update at :y inc))
                           :W (tile-facing state (update at :x dec))
                           :E (tile-facing state (update at :x inc)))]
    (when (= tile \.) new-state)))

(defn exec
  "apply `command` to state, return new state"
  [state command]
  (condp = command
    :L (update state :facing #((turn %) :L))
    :R (update state :facing #((turn %) :R))
    0 state
    (if-let [new-state (take-step state)]
      (recur new-state (dec command))
      state)))

(defn parse-path [s]
  (map (fn [c] (condp = c
                 "R" :R
                 "L" :L
                 (Integer/parseInt c)))
    (re-seq #"[\d]+|L|R" s)))

(defn initial-state []
  (let [max-x (apply max (map :x (keys *drawing*)))]
    {:facing :E
     :csize (if (< max-x 20) 4 50)
     :at {:x (->> *drawing*
               (keys)
               (filter #(zero? (:y %)))
               (map :x)
               (apply min))
          :y 0}}))

(defn make-score [state]
  (let [row (inc (get-in state [:at :y]))
        col (inc (get-in state [:at :x]))
        facing ({:E 0, :S 1, :W 2, :N 3} (state :facing))]
    (+ (* row 1000) (* 4 col) facing)))

(defn solve
  "Solve the given file using *wrapper* to navigate the map."
  [file]
  (let [[drawing path] (str/split (slurp file) #"\n\n")
        [drawing _] (binding [h/*map-ignore* #{\space}] (h/make-xy-map (str/split drawing #"\n")))
        path (parse-path path)]
    (binding [*drawing* drawing] (make-score (reduce exec (initial-state) path)))))

(defn solve-1
  ([] (solve-1 "resources/2022/day22.txt"))
  ([f] (binding [*wrapper* wrap-pt1] (solve f))))

(defn solve-2
  ([] (solve-2 "resources/2022/day22.txt"))
  ([f] (binding [*wrapper* wrap-pt2-input] (solve f))))

(deftest test-stuff [] 
  (test/are [x y] (= x y)
    [10 :R 5 :L 5 :R 10 :L 4 :R 5 :L 5] (parse-path "10R5L5R10L4R5L5")
    6032 (solve-1 "resources/2022/day22.test.txt")
    5031 (binding [*wrapper* wrap-pt2-test] (solve "resources/2022/day22.test.txt"))))
