(ns advent-of-code-2018.day1)

(defn sum-numbers [ns]
  (apply + ns))

(defn first-reach [ns]
  (loop [as (cycle ns)
         s 0
         akk (sorted-set)]
    (if (contains? akk s) 
      s
      (recur (rest as) (+ s (first as)) (conj akk s)))))

(def slurp-list 
  (map #(Integer/parseInt %) (clojure.string/split-lines (slurp "resources/day1.txt"))))

(println "Day 1, Part 1: " (sum-numbers slurp-list))
(println "Day 1, Part 2: " (first-reach slurp-list))

