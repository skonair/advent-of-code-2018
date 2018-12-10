(ns advent-of-code-2018.day9
  (:require [clojure.string :as str]))

;; implementation via zipper (G. Huet)

;; [] 1 [2 3 4]  --> prev [3 2 1]  4 []
;;
;; [1] 2 [3 4] --> prev [] 1 [2 3 4]
;;
(defn prv [z]
  (let [[l c r] z]
    (if (empty? l) 
      (let [rr (reverse (conj r c))]
        [(rest rr) (first rr) '()])
      [(rest l) (first l) (conj r c)])))

;; [3 2 1] 4 [] --> next
;; [] 1 [2 3 4]
;;
;; [2 1] 3 [4] --> next
;; [3 2 1] 4 []
;;
(defn nxt [z]
  (let [[l c r] z]
    (if (empty? r)
      (let [rl (reverse (conj l c))]
        ['() (first rl) (rest rl)])
      [(conj l c) (first r) (rest r)])))

(defn rot [z n]
  (cond
    (zero? n) z
    (< n 0) (rot (prv z) (inc n))
    (> n 0) (rot (nxt z) (dec n))))

(defn ins [z v]
  (let [[l c r] z]
    [l v (conj r c)]))

(defn rmv [z]
  (let [[l c r] z]
    (if (empty? r)
      [c [(rest l) (first l) r]]
      [c [l (first r) (rest r)]])))

(defn prt [z]
  (let [[l c r] z]
    (str (str/join " " (reverse l)) " *" c "* " (str/join " " r))))

(defn nxt-zip [z c special?]
  (if special? 
    (let [[c nz] (rmv (rot z -7))]
      [nz c])
    [(ins (rot z 2) c) 0]))

(defn game [ps lm]
  (loop [m 1
         p 0
         z ['() 0 '()]
         ss  (apply merge (for [pl (range ps)] {pl 0}))]
    (if (> m lm)
      (second (first (reverse (sort-by val ss))))
      (let [special? (zero? (mod m 23))
            [nz c] (nxt-zip z m special?) 
            nss (if special? (assoc ss p (+ m c (get ss p))) ss)]
      (recur (inc m) (mod (inc p) ps) nz nss)))))

(def slurp-list (str/split-lines (slurp "resources/day9.txt")))

(time (def result (game 491 71058)))
(println "Day 9, Part 1: " result)

(time (def result (game 491 7105800)))
(println "Day 9, Part 2: " result)

