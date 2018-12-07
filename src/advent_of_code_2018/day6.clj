(ns advent-of-code-2018.day6)

(defn min-max-xy [cs]
  { :minx (apply min (map :x cs))
    :miny (apply min (map :y cs))
    :maxx (apply max (map :x cs))
    :maxy (apply max (map :y cs))})

(defn in-bounding-box [bb]
  (for [x (range (:minx bb) (int (:maxx bb))) 
        y (range (:miny bb) (inc (:maxy bb)))] 
    {:x x :y y}))

(defn abs [n]
  (if (neg? n) (- n) n))

(defn distance [a b]
  (if (or (nil? a) (nil? b))
    nil
    (+ (abs (- (:x b) (:x a))) (abs (- (:y b) (:y a))))))

(defn nearest [c cs]
  (let [nl (sort-by (partial distance c) cs)]
    (if (= (distance c (first nl)) (distance c (second nl)))
      nil
      (first nl))))

(defn areas-of [cs]
  (let [bb (min-max-xy cs)]
    (map #(nearest % cs) (in-bounding-box bb))))

(defn outsides [cs]
  (let [mm (min-max-xy cs)]
    (distinct
      (concat 
        (for [x (range (:minx mm) (:maxx mm))] (nearest {:x x :y (:miny mm)} cs))
        (for [x (range (:minx mm) (:maxx mm))] (nearest {:x x :y (:maxy mm)} cs))
        (for [y (range (:miny mm) (:maxy mm))] (nearest {:x (:minx mm) :y y} cs))
        (for [y (range (:miny mm) (:maxy mm))] (nearest {:x (:maxx mm) :y y} cs))))))

(defn area-sizes [cs]
  (let [mm (min-max-xy cs)
        areas (areas-of cs)
        os (outsides cs)]
    (sort-by second (frequencies (filter #(not (.contains os %)) areas))))) 

(defn total-distances [c cs]
  (apply + (map #(distance c %) cs)))

(defn safe-coords [cs d]
  (let [bb (min-max-xy cs)]
    (count 
      (filter #(< (total-distances % cs) d) (in-bounding-box bb)))))

(defn parse-line [line]
  (let [[s x y] (re-find #"(\d*),.(\d*)" line)]
        {:x (Integer/parseInt x) 
         :y (Integer/parseInt y)}))

(def slurp-list 
  (map parse-line (clojure.string/split-lines (slurp "resources/day6.txt"))))

(comment def slurp-list (list {:x 1, :y 1} {:x 1, :y 6} {:x 8, :y 3} {:x 3, :y 4} {:x 5, :y 5} {:x 8, :y 9}))

(println "Day 6, Part 1: " (second (first (reverse (area-sizes slurp-list)))))
(println "Day 6, Part 2: " (safe-coords slurp-list 10000))

