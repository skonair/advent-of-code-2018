(ns advent-of-code-2018.day11)

(def MAXX 300)
(def MAXY 300)

(defn- huns [n]
  (quot (mod n 1000) 100))

(defn grid [id]
  (into [] 
  (for [y (range MAXX)
        x (range MAXY)]
    (let [rack-id (+ x 10)
          e (- (huns (* rack-id (+ id (* y rack-id)))) 5)]
     e))) )

(def grid18 (grid 9110))

(def getxy (memoize (fn [x y]
  (nth grid18 (+ x (* MAXX y))))))

(defn square-energie [x y gridsize]
  (for [dx (range gridsize)
        dy (range gridsize)]
    (getxy (+ x dx) (+ y dy)))) 

(defn rest-energie [x y gridsize]
  (apply + (concat
    (for [dy (range gridsize)] (getxy (+ x (dec gridsize)) (+ y dy)))
    (for [dx (range (dec gridsize))] (getxy (+ dx x) (+ y (dec gridsize)))))))

(def square-sum 
  (memoize 
    (fn [x y gridsize]
      (if (= 1 gridsize) 
        (getxy x y)
        (+ (square-sum x y (dec gridsize)) (rest-energie x y gridsize))))))

(defn highest-square [gridsize]
  (first
    (reverse
      (sort-by first
        (for [x (range (- MAXX gridsize))
              y (range (- MAXY gridsize))]
          [(square-sum x y gridsize) x y])))))

(defn highest-all []
  (first
    (reverse
      (sort-by first
        (for [i (range 1 MAXX)]
          (let [hs (highest-square i)]
            (flatten (vector hs i))))))))

(println "Day 11, Part 1: " ) 
(time (println "Highest square " (highest-square 3)))

(println "Day 11, Part 2: " )
(time (println "Highest all " (highest-all )))


