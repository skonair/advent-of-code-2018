(ns advent-of-code-2018.day14)

(defn n2dl [n]
  (loop [i n
         akk '()]
    (if (< i 10) (conj akk i)
      (recur (quot i 10) (conj akk (mod i 10))))))

(defn new-recipe [recipes p1 p2]
  (let [s (+ (nth recipes p1) (nth recipes p2))]
    (n2dl s)))

(defn round [recipes p1 p2]
  (let [m1 (inc (nth recipes p1))
        m2 (inc (nth recipes p2))
        new-recipes (into recipes (new-recipe recipes p1 p2))
        rlen (count new-recipes)]
    [new-recipes (mod (+ p1 m1) rlen) (mod (+ p2 m2) rlen)]))

(defn rounds [recipes n]
  (loop [rcp recipes
         p1 0
         p2 1] 
    (if (< n (count rcp))
      [rcp p1 p2]
      (let [[nxt-rcp nxt-p1 nxt-p2] (round rcp p1 p2)]
        (recur nxt-rcp nxt-p1 nxt-p2)))))

(defn score [recipes n]
  (let [[rcp p1 p2] (rounds recipes (+ n 10))]
    (clojure.string/join "" (take 10 (drop n rcp)))))

(defn ends-with? [recipes pattern]
  (if (<= (count recipes) (count pattern))
    false
    (let [lr (count recipes) 
          lp (count pattern)
          s1 (- lr lp) ]
    (or
      (= pattern (subvec recipes s1))
      (= pattern (subvec recipes (dec s1) (dec lr)))))))

(defn prefix-length [recipes pattern]
  (loop [rcp recipes
         p1 0
         p2 1] 
    (if (ends-with? rcp pattern)
      (let [d (- (count rcp) (count pattern))]
        (if (= pattern (subvec rcp d)) d (dec d))) 
      (let [[nxt-rcp nxt-p1 nxt-p2] (round rcp p1 p2)]
        (recur nxt-rcp nxt-p1 nxt-p2)))))

(println "Day 14, Part 1: " (score [3 7] 5))
(println "Day 14, Part 2: " (prefix-length [3 7] [5 1 3 4 0 1]))


