(ns advent-of-code-2018.day12
  (:require [clojure.string :as str]))

(defn pattern [state i]
  (str/join ""
    (map #(if % \# \.)
      (for [k (range -2 3)]
        (some #(= % (+ i k)) state)))))

(defn round [state rules]
  (let [mn (apply min state)
        mx (apply max state)]
    (loop [i (- mn 3)
           akk #{}]
      (if (== i (+ mx 4))
        akk
        (let [pt (pattern state i) 
              new-akk (if (some #(= % pt) rules) (conj akk i) akk)]
          (recur (inc i) new-akk))))))

(defn rounds [initial-state rules n]
  (loop [i n
         state initial-state]
    (if (zero? i)
      (apply + state)
      (recur (dec i) (round state rules)))))

(defn fifty-billion-run  [initial-state rules n]
  (loop [i 0
         state initial-state
         cnt (apply + initial-state)
         sums cnt
         delta 0]
    (let [next-state (round state rules)
          nxt-cnt (apply + next-state)
          d (- nxt-cnt cnt)]
    (if (= delta d)
      (+ sums (* d (- 50000000000 i)))
      (recur (inc i) next-state nxt-cnt (+ sums d) d)))))

(def slurp-list 
  (clojure.string/split-lines (slurp "resources/day12.txt")))

(def initial-state (str/join "" (drop 15 (first slurp-list))))
(def input (filter #(not (nil? %)) (map-indexed (fn [idx elem] (if (= elem \#) idx nil))  initial-state)))
(def rules-vec (map #(vector (str/join "" (take 5 %)) (str/join "" (drop 9 %))) (drop 2 slurp-list)))
(def rules (map first (filter #(= (second %) "#" ) rules-vec)))

(println "Day 12, Part 1: " (rounds input rules 20)) 
(println "Day 12, Part 2: " (fifty-billion-run input rules 200))

