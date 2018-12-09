(ns advent-of-code-2018.day9)

(defn insert-vec [v e p]
  (apply conj (take-last (- (count v) p) v) e (reverse (take p v))))

(defn game [players last-marble]
  (loop [marble 1
         player 0
         marbles [0]
         pos 0
         scores  (apply merge (for [p (range players)] {p 0}))]
    (comment println "Marbles[" player "] with pos(" pos ") = " marbles)
    (if (> marble last-marble)
      scores
      (let [special? (zero? (mod marble 23))
            next-pos (if special? (mod (- pos 7) (count marbles)) (mod (+ pos 2) (count marbles)))
            next-marbles (if special? (remove #{(nth marbles (inc next-pos))} marbles) (insert-vec marbles marble (inc next-pos)))
            next-scores (if special? (assoc scores player (+ marble (nth marbles (inc next-pos)) (get scores player))) scores)
            ]
      (recur 
        (inc marble)
        (mod (inc player) players)
        next-marbles
        next-pos
        next-scores)))))
        

(def slurp-list 
  (clojure.string/split-lines (slurp "resources/day9.txt")))

(def result (game 9 25))
(def result (game 10 1618))
(def result (game 13 7999))
(def result (game 17 1104))
(def result (game 21 6111))
(def result (game 30 5807))
(def result (game 491 71058))

(println "Day 9, Part 1: "(second (first (reverse (sort-by val result)))))
(println "Day 9, Part 2: " )

