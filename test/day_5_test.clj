(ns day-5-test
  (:require [day-5 :as sut]
            [clojure.test :refer [deftest is]]))




(def test-input
  [{:key "FBFBBFFRLR" :row 44 :col 5 :sid 357}
   {:key "BFFFBBFRRR" :row 70 :col 7 :sid 567}
   {:key "FFFBBBFRRR" :row 14 :col 7 :sid 119}
   {:key "BBFFBBFRLL" :row 102 :col 4 :sid 820}])



(deftest check-seat-row-test
  (is (=  test-input
          (map #(sut/key->seat-map %) (map #(:key %) test-input)))))
