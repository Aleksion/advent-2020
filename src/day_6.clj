(ns day-6
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))




;;
;; Inputs
;;
(defn load-input
  []
  (->
    (io/resource "day_6_input.txt")
    (slurp)
    ))

;;
;; Solvers
;;
(defn group->any-count
  "Will count the number of positive responses for a line"
  [l]
  (->
    (str/replace l "\n" "")
    (distinct)
    count
    )
  )

(defn group->every-count
  "Will count the number of positive responses for a line"
  [l]
  (->>
    (str/split-lines l)
    (map #(set (distinct %)))
    (apply set/intersection)
    count
    )
  )

(defn sum-responses-any
  "Will sum the counts of answers where anyone in each group answered yes"
  [i]
  (->>
    (str/split i #"\n\n")
    (map #(group->any-count %))
    (reduce + 0)))


(defn sum-responses-every
  "Will sum the counts of answers in each group were everyone answered yes"
  [i]
  (->>
    (str/split i #"\n\n")
    (map #(group->every-count %))
    (reduce + 0)))



(comment
  (println "Ready!")
  (str/split (load-input) #"\n\n")
  (sum-responses-any (load-input))
  (sum-responses-every (load-input))
  )
