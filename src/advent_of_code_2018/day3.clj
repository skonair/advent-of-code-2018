(ns advent-of-code-2018.day3
  (:require [clojure.set :as cs]))

(defn- rect-to-pos [r]
  (let [xr (Integer/parseInt (:x r))
        yr (Integer/parseInt (:y r))
        wr (Integer/parseInt (:w r))
        hr (Integer/parseInt (:h r))]
  (for [x (range xr (+ xr wr))
        y (range yr (+ yr hr))]
    [x y])))

(defn multi-claims [rs]
  (count (keys (filter #(> (second %) 1) (frequencies (apply concat (map rect-to-pos rs)))))))

(defn- single-claims [rs]
  (map first (filter #(= (second %) 1) (frequencies (apply concat (map rect-to-pos rs))))))

(defn overlapping-id [rs]
  (let [sc (set (single-claims rs))]
    (map :id (filter #(cs/subset? (set (rect-to-pos %)) sc) rs))))

(defn- parse-line [line]
  (let [[s id x y w h] (re-find #"#(\d*) @ (\d*),(\d*): (\d*)x(\d*).*" line)]
    {:id id :x x :y y :w w :h h}))

(def slurp-list 
  (map parse-line (clojure.string/split-lines (slurp "resources/day3.txt"))))

(println "Day 3, Part 1: " (multi-claims slurp-list))
(println "Day 3, Part 2: " (overlapping-id slurp-list))

