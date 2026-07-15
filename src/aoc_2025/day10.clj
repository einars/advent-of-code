(ns aoc-2025.day10
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.tools.trace :refer [deftrace]]
   [aoc.helpers :as h]))

(def sample-file "resources/2025/day10.sample.txt")
(def input-file "resources/2025/day10.txt")


(defn make-target-state [s]
  (let [no-brackets (subs s 1 (dec (count s)))
        conv {\. false, \# true}]
    (mapv conv no-brackets)))

(defn make-button-state [initial btns]
  (when (= \( (first btns))
    (let [numbers (set (h/to-int-list (subs btns 1 (dec (count btns)))))]
      (map-indexed (fn [idx _] (when (numbers idx) true)) initial))))


(defn make-pt2-target [wts]
  (h/to-int-list (subs wts 1 (dec (count wts)))))

(defn button-analysis [btns]
  (mapv (fn [c] (count (filter true? c)))
    (apply map vector btns)))

(defn parse-input-line [s]
  (let [[state & etc] (str/split s #" ")
        target (make-target-state state)
        initial (mapv (fn [_] false) target)
        button-states (mapv (fn [btns] (make-button-state initial btns)) (butlast etc)) ]
    {:initial-state initial
     :n (count initial)
     :target-state target
     :target-pt2 (make-pt2-target (last etc))
     :buttons button-states
     }))

(defn max-times [button state]
  (apply min (filter some? (map (fn [b s] (when b s)) button state))))

(defn button-efficacy [btn]
  (count (filter true? btn)))

(defn press-btn [state btn times]
  (mapv (fn [b s] (if b (- s times) s)) btn state)) 

(defn run-search [btns state n-so-far]

  (let [btn (first btns)]
    (if (= 1 (count btns))
      (let [max-n (max-times btn state)]
        (when (every? zero? (press-btn state btn max-n)) 
          (throw (ex-info "Success" {:result (+ n-so-far max-n)}))))

      (let [max-n (max-times btn state)]
        (doseq [n (reverse (range (inc max-n)))]
          (run-search (rest btns) (press-btn state btn n) (+ n-so-far n)))))))


(defn get-idxs-to-consider [state analysis]
  (let [possible-indices (filter #(pos? (state %)) (range (count state)))
        chosen-analysis (analysis (first (sort-by analysis possible-indices)))
        matching-indices (filter #(= chosen-analysis (analysis %)) possible-indices)]
    matching-indices))

(defn get-idx-to-consider [state analysis]
  (let [possible-indices (filter #(pos? (state %)) (range (count state)))]
    (first (sort-by analysis possible-indices))))

(defn runc [state btns times]
  ; generate a list of valid states that are achieved
  ; by pressing buttons in btns in total of TIMES times
  (when-not (some neg? state)
    (cond
      (empty? btns)
      []

      (= 1 (count btns))
      (let [new-state (press-btn state (first btns) times)]
        (when-not (some neg? new-state)
          [new-state]))

      :else
      (filter some? (loop [n 0, accu []]
                      (if (> n (inc (max-times (first btns) state)))
                        accu

                        (let [new-state (press-btn state (first btns) n)]
                          (if (some neg? new-state)
                            accu
                            (recur (inc n) (into accu (runc new-state (rest btns) (- times n))))))))))))

(def ^:dynamic *result* (atom nil))

(defn contradiction? [state remaining-btns]
  (let [len (count state)
        used-keys (set (mapcat (fn [btn] (filter (vec btn) (range len))) remaining-btns))
        needed-keys (set (filter #(pos? (state %)) (range len)))
        insufficient-buttons? (seq (set/difference needed-keys used-keys))
        wrong-counts? (when (= 1 (count remaining-btns)) 
                        (let [distinct-counts (set (filter pos? state))]
                          (< 1 (count distinct-counts))))]
    (or insufficient-buttons? wrong-counts?)))

;(contradiction? [1 2 0] [ [true true nil] ])
;(contradiction? [3 5 4 7] ['(nil nil nil true) '(nil true nil true) '(nil nil true nil) '(nil nil true true) '(true nil true nil) '(true true nil nil)])

(defn consider! [n]
  (when (or (nil? @*result*) (> @*result* n))
    (prn :> n)
    (reset! *result* n)))

(defn worth-considering? [n]
  (or (nil? @*result*) (> @*result* n)))

(defn runur [state btn-pool n-so-far]
  (when (and (seq btn-pool) (worth-considering? n-so-far) (not (contradiction? state btn-pool)) )
    (let [consider-idx (get-idx-to-consider state (button-analysis btn-pool))
          times (nth state consider-idx)]

      (when (worth-considering? (+ n-so-far times))
        (let [matching-btns (filterv #(nth % consider-idx) btn-pool)
              remaining-btns (filterv (complement (set matching-btns)) btn-pool)
              new-states (sort-by #(reduce + %) (runc state matching-btns times))
              winning-states (filter #(every? zero? %) new-states)]
          (when (seq winning-states)
            ;(+ times n-so-far)
            ;(throw (ex-info "Success" {:result (+ times n-so-far)}))
            (consider! (+ times n-so-far)))

          (mapv #(runur % remaining-btns (+ times n-so-far)) (filter #(some pos? %) new-states)))))))


(defn find-min-pt2 [p]
  (try
    (binding [*result* (atom nil)]
      (runur (:target-pt2 p) (:buttons p) 0)
      (prn (:target-pt2 p) @*result*)
      @*result*)
    (catch Exception e
      (if-let [r (:result (ex-data e))]
        (do
          (prn r (:target-pt2 p))
          r)
        (throw e)))))



(defn parse-input [f]
  (mapv parse-input-line (h/slurp-strings f)))

(defn pt2 [f]
  (prn :----)
  (let [d (parse-input f)]
    (reduce + (mapv find-min-pt2 d))))

;(pt2 sample-file)
;(pt2 input-file)

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
                               existing-cost (get-in pool [new-state :n] nil)]
                         :when  (or (nil? existing-cost) (< new-cost existing-cost))]
                     [new-state 
                      {:n new-cost :path (conj (get-in pool [old-state :path] []) b)}])
         new-pool (into pool new-stuff)]
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
  ([f] (pt1 f)))

(defn solve-2 
  ([] (solve-2 input-file))
  ([f] (pt2 f)))

(deftest tests []
  (are [x y] (= x y)
    33 (pt2 sample-file)))

(comment
  (solve-1)
  (solve-2))
