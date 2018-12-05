(ns advent-of-code-2018.day5
  (:require [clojure.string :as str]))

(defn- pair? [x y]
  (if (or (nil? x) (nil? y))
    false
    (= 32 (bit-xor (int x) (int y)))))

(defn reduce-units [c]
  (loop [[x & rs] c
         akk []]
    (let [p (pair? x (first rs))]
      (if (nil? x) 
        (apply str akk)
        (recur (if p (drop 1 rs) rs) (if p akk (conj akk x)))))))

(defn strip [coll chars]
    (apply str (remove #((set chars) %) coll)))

(defn reduce-text [c & [ru]]
  (loop [cs (if ru (strip c (str ru (str/upper-case ru))) c)
         s 0]
    (if (= s (count cs)) 
       cs
      (recur (reduce-units (vec cs)) (count cs)))))

(defn reduce-text-with-char [c soc]
  (first 
    (sort-by second
      (map #(let [t (reduce-text c %)] (vector % (count t))) soc))))

(def slurp-list (str/trim-newline (slurp "resources/day5.txt")))
(def set-of-chars (set (str/lower-case slurp-list)))

(println "Day 5, Part 1: " (count (reduce-text slurp-list)) )
(println "Day 5, Part 2: " (reduce-text-with-char slurp-list set-of-chars))

