(ns advent-of-code-2018.day10
  (:require [clojure.string :as str]))

(defn round [input]
  (map #(hash-map :px (+ (:vx %) (:px %)) :py (+ (:vy %) (:py %)) :vx (:vx %) :vy (:vy %)) input))

(defn in2n [c e]
  (if (some #(= % e) c) 1 0))

(defn in? [c e]
  (some #(= % e) c))

(defn neighbours [x y i]
  (let [result (+ 
                 (in2n i {:px (dec x) :py (dec y)}) 
                 (in2n i {:px x :py (dec y)}) 
                 (in2n i {:px (inc x) :py (dec y)}) 
                 (in2n i {:px (dec x) :py y}) 
                 (in2n i {:px (inc x) :py y}) 
                 (in2n i {:px (dec x) :py (inc y)}) 
                 (in2n i {:px x :py (inc y)}) 
                 (in2n i {:px (inc x) :py (inc y)}))] 
    result))

(defn no-neighbours-exist? [i]
  (let [nes (filter #(zero? (neighbours (:px %) (:py %) i)) i)]
    (zero? (count nes))))

(defn- s2i [s]
   (Integer/parseInt (str/trim s)))

(defn parse-line [line]
  (let [[s px py vx vy] (re-find #"position=<([ \-]?\d*),\ ?([ \-]?\d*)>.velocity=<([ \-]?\d*),\ ?([ \-]?\d*)>" line)]
    {:px (s2i px) :py (s2i py) :vx (s2i vx) :vy (s2i vy)}))

(defn i2a [input]
  (map #(hash-map :px (:px %) :py (:py %)) input))

(defn i2ca [input]
  (let [minx (apply min (map #(:px %) input))
        maxx (apply max (map #(:px %) input))
        miny (apply min (map #(:py %) input))
        maxy (apply max (map #(:py %) input))
        cnv (i2a input)]
    (for [y (range miny (inc maxy))
          x (range minx (inc maxx)) ]
      (let [lb (if (= maxx x) "\n" "")]
        (if (in? cnv {:px x :py y}) (str "#" lb) (str "." lb))))))

(defn roundx [i x]
  (println "roundx " x " (count i) = " (count i))
  (if (zero? x) 
    i
    (recur (round i) (dec x))))

(defn check-rounds [i n]
  (if (zero? (mod n 100)) (println "N is " n)) 
  (if (no-neighbours-exist? (i2a i))
    (println (i2ca i) "after " n " iterations")
    (recur (round i) (inc n))))

(def input 
  (map parse-line (clojure.string/split-lines (slurp "resources/day10.txt"))))

(println "Day 10, Part 1: ")
(println "Day 10, Part 2: ")

(def rx (roundx input 10200)) ;; cycle without check to speed things up
(check-rounds rx 10200)

