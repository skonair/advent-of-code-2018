(ns advent-of-code-2018.day2)

(defn- dup-chars? [line n]
  (some #(= n (second %)) (frequencies line)))

(defn- dups-in-lines [lines n]
  (count (filter #(dup-chars? % n) lines)))

(defn checksum [lines]
    (* (dups-in-lines lines 2) (dups-in-lines lines 3)))

(defn str-diff [s1 s2]
  (loop [[a & as] s1
         [b & bs] s2
         res ""]
    (if (or (nil? a) (nil? b))
            res
    (recur as bs (str res (if (= a b) a nil))))))

(defn check-for-one [l lines]
  (filter #(= 1 (- (count l) (count (str-diff l %)))) lines))

(defn diff-by-two [lines]
  (apply str-diff (flatten (map #(check-for-one % lines) lines))))


(def slurp-list 
  (clojure.string/split-lines (slurp "resources/day2.txt")))

(println "Day 2, Part 1: " (checksum slurp-list)) 
(println "Day 2, Part 2: " (diff-by-two slurp-list))

