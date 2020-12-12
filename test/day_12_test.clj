(ns day-12-test
  (:require [day-12 :as sut]
            [clojure.test :refer [deftest is]]))


(def test-input (sut/load-input "test/fixtures/day_12_input.txt"))

(deftest solve-1-test
  (is (= 25
         (sut/solve-1 test-input))))

(deftest solve-2-test
  (is (= 286
         (sut/solve-2 test-input))))
