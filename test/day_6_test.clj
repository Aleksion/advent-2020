(ns day-6-test
  (:require [day-6 :as sut]
            [clojure.test :refer [deftest is]]))


(def test-input "abc

a
b
c

ab
ac

a
a
a
a

b")



(deftest group->any-count-test
  (is (and
        (= 3
           (sut/group->any-count "abc"))
        (= 3
           (sut/group->any-count
             "a
b
c
"))
        )))


(deftest sum-responses-any-test
  (is (= 11
         (sut/sum-responses-any test-input))))

(deftest sum-responses-every-test
  (is (= 6
         (sut/sum-responses-every test-input))))
