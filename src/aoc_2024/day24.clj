(ns aoc-2024.day24
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.string :as str]
   [clojure.tools.trace :refer :all]
   [clojure.set :as set]
   [instaparse.core :as insta]
   [aoc.helpers :as h]))

(def input-txt "resources/2024/day24.txt")

(def sample
  "x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj")

(defn tree-parse-reg
  [tree]
  (cond
    (and
      (coll? tree)
      (= :reg (first tree))) (keyword (second tree))
    (coll? tree) (mapv tree-parse-reg tree)
    :else tree))

(def parser (insta/parser "
<task>   = sets <nl> ops <nl>?
sets = (set <nl>)+
ops = op (<nl> op)*
set = reg <': '> bit
<bit> = '0' | '1'
op = reg <' '> (and|or|xor) <' '> reg <' -> '> reg
reg = #'[a-z][a-z\\d][a-z\\d]'
and = <'AND'>
or = <'OR'>
xor = <'XOR'>
nl       = '\n'
"))


(defn parse-input
  [s]
  (let [[[_ & sets] [_ & ops]] (tree-parse-reg (parser s))]
    [sets ops]))

(parse-input sample)

(def as-bool {"1" true, "0" false})

(defn set-initial 
  [[x & rest] accu]
  (cond
    (nil? x) accu
    (= :set (first x)) (recur rest (assoc accu (second x) (as-bool (nth x 2))))))

(defn apply-op [op ra rb]
  (condp = op
    :and (and ra rb)
    :or (or ra rb)
    :xor (not= ra rb)))

(defn print-solution [data]
  (let [bin-bools (map data (reverse (sort-by identity (filter #(str/starts-with? (name %) "z") (keys data)))))]
    (loop [[bit & rest] bin-bools, accu 0]
      (cond 
        (nil? bit) accu
        (false? bit) (recur rest (* accu 2))
        (true? bit) (recur rest (inc (* accu 2))) ) )))

(defn pt1 [[sets ops]]
  (let [initial (set-initial sets {})]
    (loop [[op & rest] ops, settled? true, data initial]

      (let [[_ ra [rop] rb rtarget] op
            xa (data ra)
            xb (data rb)
            xtarget (data rtarget)]
        ;(prn :ra ra xa :rb rb xb :target rtarget xtarget )
        (cond
          (and (nil? op) (not settled?)) (recur ops true data)
          (and (nil? op) settled?) (print-solution data)
          (some? xtarget) (recur rest settled? data)
          (and (some? xa) (some? xb)) (recur rest false (assoc data rtarget (apply-op rop xa xb)))
          :else (recur rest settled? data)
          )))))

;(second (parse-input (slurp input-txt)))



(defn make-graphviz [[inp ops]]
  (let [
        allnodes (reduce (fn [s [_ a op b tgt]] (conj s a b tgt)) #{} ops)
        allnodes (set (map name allnodes))
        xnodes (filter #(str/starts-with? (name %) "x") allnodes)
        ynodes (filter #(str/starts-with? (name %) "y") allnodes)
        znodes (filter #(str/starts-with? (name %) "z") allnodes)
        restnodes (filter (complement (set/union (set xnodes) (set ynodes) (set znodes))) (set allnodes))
        xnodes (sort xnodes)
        ynodes (sort ynodes)
        znodes (sort znodes)
        restnodes (sort restnodes)
        ]

    (format "
digraph Schematic {
  nslimit=1000;
  rankdir=LR;
  node [shape=circle];
  { rank=same; %s } // xnodes
  { rank=same; %s } // ynodes
  { rank=same; %s } // znodes
  %s
}" 
      (str/join "; " xnodes)
      (str/join "; " ynodes)
      (str/join "; " znodes)
      (apply str (map (fn [[_ a [op] b t]] 
                        (format "
gate_%s [label=\"%s\", shape=box];
%s -> gate_%s;
%s -> gate_%s;
gate_%s -> %s; "
                          (name t) op
                          (name a) (name t)
                          (name b) (name t)
                          (name t) (name t)) ) ops))
      ) ))


(spit "/proj/aoc/resources/2024/day24.dot" (make-graphviz (parse-input (slurp input-txt))))

(defn solve-1
  ([] (solve-1 (slurp input-txt)))
  ([ss] (pt1 (parse-input ss))))

(defn solve-2
  ([] (solve-2 (slurp input-txt)))
  ([ss] (pt2 (parse-input ss))))

(deftest tests []
  (are [x y] (= x y)
    2024 (pt1 (parse-input sample))
    2 (pt2 (parse-input sample))))

(comment
  (solve-1)
  (solve-2))
