(ns advent-of-code-2018.day13)

(defn parse-line [line line-nr]
  (filter #(not (= \  (second %)))
    (map-indexed (fn [idx e] (vector (vector idx line-nr) e)) line)))

(def slurp-list 
  (clojure.string/split-lines (slurp "resources/day13.txt")))

(def track2 
    (apply concat (map-indexed (fn [idx e] (parse-line e idx)) slurp-list)))
(def carts1 (filter (fn [e] (some #(= % (second e)) '(\^ \v \< \>))) track2))
(def carts (map #(conj % 0) carts1))
(def track (into {} track2))

(defn next-intersection [prev-cart number-of-intersections]
  (let [m (mod number-of-intersections 3)
        ios (inc number-of-intersections)]
    (cond
      (and (= m 0) (= prev-cart \^)) [\< ios]
      (and (= m 0) (= prev-cart \>)) [\^ ios]
      (and (= m 0) (= prev-cart \v)) [\> ios]
      (and (= m 0) (= prev-cart \<)) [\v ios]
      (and (= m 2) (= prev-cart \^)) [\> ios]
      (and (= m 2) (= prev-cart \>)) [\v ios]
      (and (= m 2) (= prev-cart \v)) [\< ios]
      (and (= m 2) (= prev-cart \<)) [\^ ios]
      :otherwise [prev-cart ios])))

(defn next-cart [prev-cart next-track number-of-intersections]
  (cond
    (and (= prev-cart \^) (= next-track \\)) [\< number-of-intersections]
    (and (= prev-cart \^) (= next-track \/)) [\> number-of-intersections]
    (and (= prev-cart \v) (= next-track \\)) [\> number-of-intersections]
    (and (= prev-cart \v) (= next-track \/)) [\< number-of-intersections]
    (and (= prev-cart \<) (= next-track \\)) [\^ number-of-intersections]
    (and (= prev-cart \<) (= next-track \/)) [\v number-of-intersections]
    (and (= prev-cart \>) (= next-track \\)) [\v number-of-intersections]
    (and (= prev-cart \>) (= next-track \/)) [\^ number-of-intersections]
    (and (= next-track \+)) (next-intersection prev-cart number-of-intersections)
    :otherwise [prev-cart number-of-intersections]))

(defn next-pos [pos cart]
  (cond
    (= cart \^) [(first pos) (dec (second pos))]
    (= cart \v) [(first pos) (inc (second pos))]
    (= cart \<) [(dec (first pos)) (second pos)]
    (= cart \>) [(inc (first pos)) (second pos)]))

(defn get-track [pos track]
  (let [track-element (get track pos)]
    (cond 
      (= track-element \v) \|
      (= track-element \^) \|
      (= track-element \<) \-
      (= track-element \>) \-
      :otherwise track-element)))

(defn crash-pos [carts]
  (let [positions (map first carts)]
    (if (not (= (count positions) (count (distinct positions))))
      (first (last (sort-by second (frequencies positions))))
      nil)))

(defn crash? [pos carts]
    (> (count (filter #(= pos (first %)) carts)) 0))

(defn tick [track carts]
  (let [sorted-carts (sort-by first carts)]
    (loop [[cart & cs] sorted-carts
           akk '()
           crashes [] ]
      (if (nil? cart)
        [akk crashes]
        (let [[pos c no] cart
              np (next-pos pos c)
              nt (get-track np track)
              [nc nnos] (next-cart c nt no)
              new-akk (conj akk (vector np nc nnos))
              new-crashes (if (crash? np (concat akk cs)) (conj crashes np) crashes)
              new-crashes-op (if (crash? pos (concat akk cs)) (conj new-crashes np) new-crashes) ]
          (recur cs new-akk new-crashes-op))))))

(defn tick-to-crash [track carts]
  (loop [cs carts]
    (let [[next-carts crashes] (tick track cs)]
      (if (not (empty? crashes))
       (first crashes)
       (recur next-carts)))))

(defn cart-in-pos? [cart positions]
  (some #(= % (first cart)) positions))

(defn remove-crashed [carts crashed]
  (filter #(not (cart-in-pos? % crashed)) carts))

(defn tick-to-crash2 [track carts]
  (loop [cs carts]
    (let [[next-carts crashes] (tick track cs)]
      (if (< (count cs) 2)
       (first (first cs))
       (recur (remove-crashed next-carts crashes))))))

(println "Day 13, Part 1: " (tick-to-crash track carts)) 
(println "Day 13, Part 2: " (tick-to-crash2 track carts))

