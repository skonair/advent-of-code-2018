(ns advent-of-code-2018.day14-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2018.day14 :refer :all]))

(deftest part1-test []
         (testing "After 9 recipes, the scores of the next ten would be 5158916779." (is (= "5158916779" (score [3 7] 9))))
         (testing "After 5 recipes, the scores of the next ten would be 0124515891." (is (= "0124515891" (score [3 7] 5))))
         (testing "After 18 recipes, the scores of the next ten would be 9251071085" (is (= "9251071085" (score [3 7] 18))))
         (testing "After 2018 recipes, the scores of the next ten would be 5941429882" (is (= "5941429882" (score [3 7] 2018)))))

(deftest part2-test []
         (testing "51589 first appears after 9 recipes." (is (= 9 (prefix-length [3 7] [5 1 5 8 9]))))
         (testing "01245 first appears after 5 recipes." (is (= 5 (prefix-length [3 7] [0 1 2 4 5]))))
         (testing "92510 first appears after 18 recipes." (is (= 18 (prefix-length [3 7] [9 2 5 1 0]))))
         (testing "59414 first appears after 2018 recipes." (is (= 2018 (prefix-length [3 7] [5 9 4 1 4])))))




