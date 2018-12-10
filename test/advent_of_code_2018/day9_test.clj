(ns advent-of-code-2018.day9-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2018.day9 :refer :all]))

(deftest round-test [] 
         (testing "10 players; last marble is worth 1618 points: high score is 8317" (is (= 8317 (game 10 1618))))
         (testing "13 players; last marble is worth 7999 points: high score is 146373" (is (= 146373 (game 13 7999))))
         (testing "17 players; last marble is worth 1104 points: high score is 2764" (is (= 2764 (game 17 1104))))
         (testing "21 players; last marble is worth 6111 points: high score is 54718" (is (= 54718 (game 21 6111))))
         (testing "30 players; last marble is worth 5807 points: high score is 37305" (is (= 37305 (game 30 5807))))
         (testing "9 players; last marble is worth 25 points: high score is 32" (is (= 32 (game 9 25)))))
