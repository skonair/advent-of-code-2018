(ns advent-of-code-2018.day4
  (:require [clojure.string :as str]))

(defn- get-guard [l]
  (let [[a guard] (re-find #"Guard #(\d*) begins shift" l)]
    guard))

(defn sleeptime [lines]
  (loop [lns lines
         guard nil
         begin nil
         akk {}]
    (let [l (first lns) ls (rest lns)]
      (if (nil? l) 
        akk
        (recur 
          ls 
          (if (re-find #"Guard" (:msg l)) (get-guard (:msg l)) guard)
          (if (re-find #"falls asleep" (:msg l)) (:minute l) begin)
          (if (re-find #"wakes up" (:msg l)) 
            (assoc akk guard (concat (get akk guard) (range begin (:minute l))))
            akk))))))

(defn get-most-frequent-vals [items]
    (->> items
          frequencies
          (sort-by val)
          reverse
          first))

(defn frequency-stats [st] 
  (let [mf (get-most-frequent-vals (second st))]
    (vector
      (count (second st))
      (second mf)
      (first mf)
      (Integer/parseInt (first st)))))

(defn black-magic-fuckery [fkt lines]
  (->> lines
    sleeptime
    (map frequency-stats)
    (sort-by fkt)
    reverse
    first
    (drop 2)
    (apply *)))

(defn- parse-line [line]
  (let [[r yyyy m dd hh mm msg] (re-find #".(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})..(.*)" line)]
    {:minute (Integer/parseInt mm) :msg msg}))

(def slurp-list 
  (map parse-line (sort (str/split-lines (slurp "resources/day4.txt")))))

(println "Day 4, Part 1: " (black-magic-fuckery first slurp-list))
(println "Day 4, Part 2: " (black-magic-fuckery second slurp-list))

