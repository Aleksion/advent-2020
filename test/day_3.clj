(ns day-3-test
  (:require [day-3 :as sut]
            [clojure.test :refer [deftest is]]))



(def test-input
  "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#
")



(deftest n-of-trees-on-path-test
  (is (=
        7
        (sut/n-of-trees-on-path test-input [3 1])
        )))


(deftest check-trees-on-slopes-test
  (is (= 336
         (sut/check-trees-on-slopes test-input)
         ))
  )
