(ns aoc-2025.day10
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [clojure.tools.trace :refer [deftrace]]
   [aoc.helpers :as h]))

(def sample-file "resources/2025/day10.sample.txt")
(def input-file "resources/2025/day10.txt")


(defn make-target-state [s]
  (let [no-brackets (subs s 1 (dec (count s)))
        conv {\. false, \# true}]
    (mapv conv no-brackets)))

(deftrace make-button-state [initial btns wts]
  (when (= \( (first btns))
    (let [numbers (set (h/to-int-list (subs btns 1 (dec (count btns)))))]
      (map-indexed (fn [idx _] (when (numbers idx) (nth wts idx))) initial))))


(defn make-button-weights [wts]
  (h/to-int-list (subs wts 1 (dec (count wts)))))

(defn parse-input-line [s]
  (let [[state & etc] (str/split s #" ")
        target (make-target-state state)
        initial (mapv (fn [_] false) target)
        button-weights (make-button-weights (last etc))
        button-states (mapv (fn [btns] (make-button-state initial btns button-weights)) (butlast etc)) ]
    {:initial-state initial
     :target-state target
     :buttons button-states }))

(defn parse-input [f]
  (mapv parse-input-line (h/slurp-strings f)))

(defn apply-button-press [combo btns]
  (mapv (fn [cur bb]
          (if-not bb
            cur
            (not cur))) combo btns))

(defn rs [s]
  (apply str (mapv {false "-", true "#"} s)))

(defn explore-clicks
  ([p] (explore-clicks p { (:initial-state p) {:n 0 :path []} }))
  ([p pool]
   (let [new-stuff (for [old-state (keys pool)
                         b (:buttons p)
                         :let [new-state (apply-button-press old-state b)
                               new-cost (+ 1 (get-in pool [old-state :n])) 
                               existing-cost (get-in pool [new-state :n] nil)
                               ]
                         :when  (or (nil? existing-cost) (< new-cost existing-cost))]
                     [new-state 
                      {:n new-cost :path (conj (get-in pool [old-state :path] []) b)}])
         new-pool (reduce (fn [p [k v]] (assoc p k v)) pool new-stuff)
         ;new-pool (into pool new-stuff)
         ]
     (if (seq new-stuff)
       (recur p new-pool)
       new-pool))))




(defn pt1 [f]
  (let [probs (parse-input f)]
    (reduce + (for [p probs
                    :let [all-clicks (explore-clicks p)]]
                (get-in all-clicks [(:target-state p) :n])))))





;(pt1 sample-file)

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
