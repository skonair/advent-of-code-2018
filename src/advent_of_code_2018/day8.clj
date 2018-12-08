(ns advent-of-code-2018.day8)

(defn node-value [metadatas values]
  (apply + (for [m metadatas 
                 :when (and (> m 0) (<= m (count values)))]
             (nth values (dec m)))))

(defn create-tree [input]
  (let [cn (first input)
        me (second input)
        remaining (drop 2 input)]
    (loop [i cn
           sum-of-metadata 0
           values []
           rem remaining]
      (if (zero? i)
        (let [summ (apply + (take me rem))
              som (+ sum-of-metadata summ)
              dir (drop me rem)
              nval (node-value (take me rem) values)]
        (if (zero? cn)
          [som, summ, dir]
          [som, nval, dir]))
        (let [[a b c] (create-tree rem)]
          (recur (dec i) (+ sum-of-metadata a) (conj values b) c))))))

(def slurp-list 
  (map #(Integer/parseInt %) (clojure.string/split (clojure.string/trim (slurp "resources/day8.txt")) #" ")))

(comment def slurp-list '(2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2))

(def t (create-tree slurp-list))

(println "Day 8, Part 1: " (first t))
(println "Day 8, Part 2: " (second t))

