(ns advent-of-code-2018.day7)

(defn parse-line [line]
  (let [[m b s] (re-find #"Step (\w) must be finished before step (\w) can begin." line)]
    [b s]))

(def rules 
  (map parse-line (clojure.string/split-lines (slurp "resources/day7.txt"))))

(def all-steps
  (set (distinct (concat (map first rules) (map second rules)))))

(defn in? [e c]
  (some #(= e %) c))

(defn applying-rules [step]
  (filter #(= step (second %)) rules))

(defn is-step-possible? [step akk]
  (let [rs (applying-rules step)
        preconds (map first rs)]
    (every? #(in? % akk) preconds)))

(defn available-steps [akk]
  (filter #(is-step-possible? % akk) (remove #(in? % akk) all-steps)))

(defn next-step [akk]
  (let [avails (available-steps akk)]
    (first (sort avails))))

(defn order-instructions []
  (loop [akk []]
      (if (= (count akk) (count all-steps))
        (clojure.string/join "" akk)
        (let [na (next-step akk)]
          (comment println "next-available " akk " ==> " na)
          (recur (conj akk na))))))

(defn finished-this-round [in-work t]
  (filter #(= t (second %)) in-work))

(defn step-seconds [step]
  (+ 61 (apply - (map int (str step "A")))))

(defn order-instructions2 [workers]
  (loop [akk []
         in-work []
         t -1
         free-workers (inc workers)]
    (comment println "Loop: t = " t "  with " free-workers " free workers. In work: " in-work " and finished " akk)
      (if (= (count akk) (count all-steps))
        t
        (let [finished-rnd (finished-this-round in-work t)
              newakk (concat akk (map first finished-rnd))
              avails (remove #(in? % (map first in-work)) (available-steps newakk))
              fw (+ free-workers (count finished-rnd))
              additional-in-work (take fw (map #(vector % (+ t (step-seconds %))) avails))
              new-in-work (concat additional-in-work (remove #(in? % finished-rnd) in-work)) ]
          (comment println "Avails is " avails)
          (recur newakk new-in-work (inc t) (- fw (count additional-in-work)))))))

(println "Day 2, Part 1: " (order-instructions))
(println "Day 2, Part 2: " (order-instructions2 5))

