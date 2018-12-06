(ns advent-of-code-2018.day6)

(defn min-max-xy [cs]
  (comment println "min-max-xy " cs)
  { :minx (apply min (map :x cs))
    :miny (apply min (map :y cs))
    :maxx (apply max (map :x cs))
    :maxy (apply max (map :y cs))})

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

(defn belongs? [p c cs]
  (= c (nearest p cs)))

(defn areas-of [cs]
  (let [bb2 (min-max-xy cs)
        bb {:minx 0 :maxx 400 :miny 0 :maxy 400}]
    (map #(nearest % cs) (for [x (range (:minx bb) (int (:maxx bb))) y (range (:miny bb) (inc (:maxy bb)))] {:x x :y y}))))

(defn area-of [c cs]
  (let [bb (min-max-xy cs)]
    (filter #(belongs? % c cs) (for [x (range (:minx bb) (:maxx bb)) y (range (:miny bb) (:maxy bb))] {:x x :y y}))))

(defn outside? [c mm]
  (or
    (= (:x c) (:minx mm))
    (= (:y c) (:miny mm))
    (= (:x c) (:maxx mm))
    (= (:y c) (:maxy mm))))

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

(defn parse-line [line]
  (let [[s x y] (re-find #"(\d*),.(\d*)" line)]
        {:x (Integer/parseInt x) 
         :y (Integer/parseInt y)}))

(def slurp-list 
  (map parse-line (clojure.string/split-lines (slurp "resources/day6.txt"))))

(comment def slurp-list (list {:x 1, :y 1} {:x 1, :y 6} {:x 8, :y 3} {:x 3, :y 4} {:x 5, :y 5} {:x 8, :y 9}))

(println "Outsides: " (outsides slurp-list))

(println "Day 6, Part 1: " (area-sizes slurp-list)) 
(println "Day 6, Part 2: " )

