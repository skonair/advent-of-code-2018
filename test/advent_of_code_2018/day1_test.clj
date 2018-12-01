(ns advent-of-code-2018.day1-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2018.day1 :refer :all]))

(deftest sum-number-test []
         (testing "+1, +1, +1 results in  3" (is (= 3 (sum-numbers '(+1, +1, +1)))))
         (testing "+1, +1, -2 results in  0" (is (= 0 (sum-numbers '(+1, +1, -2)))))
         (testing "-1, -2, -3 results in -6" (is (= -6 (sum-numbers '(-1, -2, -3))))))

(deftest first-reach-test []
         (testing "+1, -1 first reaches 0 twice" (is (= 0 (first-reach '(+1, -1)))))
         (testing "+3, +3, +4, -2, -4 first reaches 10 twice" (is (= 10 (first-reach '(+3, +3, +4, -2, -4)))))
         (testing "-6, +3, +8, +5, -6 first reaches 5 twice" (is (= 5 (first-reach '(-6, +3, +8, +5, -6)))))
         (testing "+7, +7, -2, -7, -4 first reaches 14 twice" (is (= 14 (first-reach '(+7, +7, -2, -7, -4))))))
