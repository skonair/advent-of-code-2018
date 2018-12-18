(ns advent-of-code-2018.day18)

(defn acre [c x y]
  (let [[w h l] c]
    (if (or (>= y h) (>= x w) (< y 0) (< x 0)) 
      nil
      (nth l (+ x (* y h))))))

(defn neighbours [c x y]
  (frequencies
    (filter #(not (nil? %))
      (conj []
          (acre c (dec x) (dec y))
          (acre c x (dec y))
          (acre c (inc x) (dec y))
          (acre c (dec x) y)
          (acre c (inc x) y)
          (acre c (dec x) (inc y))
          (acre c x (inc y))
          (acre c (inc x) (inc y))))))

(defn nxt [c x y]
  (let [e (acre c x y)
        n (neighbours c x y)
        co (or (get n \.) 0)
        ct (or (get n \|) 0)
        cl (or (get n \#) 0)]
    (cond 
      (= e \.) (if (>= ct 3) \| e)
      (= e \|) (if (>= cl 3) \# e)
      (= e \#) (if (and (>= cl 1) (>= ct 1)) \# \.))))

(defn prnt [c]
  (let [[w h l] c] 
    (apply str
    (for [y (range h) 
          x (range w)]
     (do 
       (if (zero? x) 
         (str "\n" (acre c x y)) 
         (acre c x y)))))))

(defn part1 [c]
  (let [[w h l] c
        fs (frequencies l)
        co (or (get fs \.) 0)
        ct (or (get fs \|) 0)
        cl (or (get fs \#) 0)]
    (* ct cl)))

(defn round [c]
  (let [[w h l] c
       nl (for [y (range h) x (range w)] (nxt c x y))]
    (vector w h nl)))

(defn rounds [c n]
  (loop [i 0
         cs c]
    (println "i: " i " --> " (part1 cs))
  (if (>= i n)
    cs
    (let [next-c (round cs)]
        (recur (inc i) next-c )))))

(def slurp-lines (clojure.string/split-lines (slurp "resources/day18.txt")))
(def lumberyard (vector (count (nth slurp-lines 0)) (count slurp-lines) (apply str slurp-lines)))

(println "Day 18, Part 1: " (part1 (rounds lumberyard 10)))
(println "Day 18, Part 2: " (part1 (rounds lumberyard 1000000000))) 

;; repetition after i = 423, cycle is 28 steps --> (1000000000 - 423) % 28 = 17 
;; ==> val(423 + 17) = 201465
;; i:  423  -->  179800
;; i:  424  -->  181853
;; i:  425  -->  185859
;; i:  426  -->  187850
;; i:  427  -->  190046
;; i:  428  -->  190740
;; i:  429  -->  186595
;; i:  430  -->  182560
;; i:  431  -->  184008
;; i:  432  -->  184032
;; i:  433  -->  184254
;; i:  434  -->  186880
;; i:  435  -->  191160
;; i:  436  -->  195000
;; i:  437  -->  198387
;; i:  438  -->  201798
;; i:  439  -->  201798
;; i:  440  -->  201465
;; i:  441  -->  199995
;; i:  442  -->  200178
;; i:  443  -->  197232
;; i:  444  -->  195460
;; i:  445  -->  191285
;; i:  446  -->  185004
;; i:  447  -->  181192
;; i:  448  -->  176484
;; i:  449  -->  178776
;; i:  450  -->  177232
;; i:  451  -->  179800

