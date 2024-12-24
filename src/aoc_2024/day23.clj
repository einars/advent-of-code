(ns aoc-2024.day23
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [clojure.tools.trace :refer :all]
   [instaparse.core :as insta]
   [aoc.helpers :as h]))

(def input-txt "resources/2024/day23.txt")

(def sample
  "kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn")

(defn add-to-set [s q]
  (if s
    (conj s q)
    #{q}))

(defn parse-and-add [accu s]
  (let [[f t] (str/split s #"-")]
    (-> accu
      (update f add-to-set t)
      (update t add-to-set f))))

(defn parse-input
  [s]
  (reduce parse-and-add {} (str/split s #"\n")))


(defn find-3 [net]
  (set 
    (remove nil?
      (flatten
        (for [computer (filter #(str/starts-with? % "t") (keys net))]
          (for [node (net computer)]
            (for [hop (net node)]
              (when ((net hop) computer)
                #{node hop computer}))))))))

(defn grow-set [members net]
  (loop [members members, [candidate & rest] (keys net)]
    (cond
      (nil? candidate) members
      (members candidate) (recur members rest)
      (every? #((net candidate) %) members) (recur (conj members candidate) rest)
      :else (recur members rest))))

(defn find-fat-set [net]
  (first (sort-by (comp - count) (mapv #(grow-set #{%} net) (keys net)))))


(defn pt1
  [task]
  (count (find-3 task)))

(defn pt2
  [task]
  (str/join "," (sort (find-fat-set task))))

(defn solve-1
  ([] (solve-1 (slurp input-txt)))
  ([ss] (pt1 (parse-input ss))))

(defn solve-2
  ([] (solve-2 (slurp input-txt)))
  ([ss] (pt2 (parse-input ss))))

(deftest tests []
  (are [x y] (= x y)
    7 (pt1 (parse-input sample))
    "co,de,ka,ta" (pt2 (parse-input sample))))

(comment
  (solve-1)
  (solve-2))
