(ns day-10-test
  (:require [day-10 :as sut]
            [clojure.test :refer [deftest is]]
            [clojure.string :as str]))



(def input (sut/load-input  "test/fixtures/day_10_input.txt"))


(deftest adapters->jolt-differences-test
  (is (=
        {:1 22 :2 0 :3 10}
        (sut/adapters->jolt-differences input))))
