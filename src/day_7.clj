(ns day-7
  (:require [clojure.string :as str]
            [malli.core :as m]
            [malli.transform :as mt]
            [clojure.java.io :as io]))


;;
;; Input
;;
(defn load-input
  []
  (-> (io/resource "day_7_input.txt")
      (slurp)
      (str/split-lines)
      ))

;;
;; Logic
;;
(defn get-name
  [s]
  (->>
    (str/split s #" ")
    (take 2)
    (str/join " ")
    ))

(defn parse-child
  [s]
  (->
    (str/replace s "." "")
    (str/trim)
    (str/split #" ")
    (#(do
        {:count (Integer/parseInt (first %))
         :name (str/join " " (subvec % 1 3))
         }
        ))
    )
  )

(defn get-children
  [s]
  (let [sub-s (re-find #"(?<=\bcontain\s).*" s)]
    (if (str/includes? sub-s "no other bags")
      []
      (->>
        (str/split sub-s #",")
        (map #(parse-child %)))
      ))
  )


(defn line->rule
  "Converts a line into a rule"
  [line]
  {
   :name (get-name line)
   :children (into [] (get-children line))
   }
  )


(defn can-contain
  "Returns a sequence of bags that can contain the provided bag"
  [coll name]
  (->>
    (map #(line->rule %) coll)
    (filter (fn [v] (some
                     #(= name (:name %))
                     (:children v))))
    (map #(:name %))
    )
  )

(defn contains
  "Returns a sequence of bags that can contain the provided bag
  Multiplier will multiply the count by the count of it's ancestors
  "
  [coll name multiplier]
  (->>
    (map #(line->rule %) coll)
    (filter #(= (:name %) name))
    (first)
    (:children)
    (map (fn [v] (update v :count #(* % multiplier)))
         )))


;;
;; Solvers
;;
(defn get-ancestors
  [rules name]
  (loop [rest (can-contain rules name) acc []]
    (if (=  (count rest) 0)
      acc

      (let [ancs (reduce #(set (concat %1 (can-contain rules %2))) [] rest)]
        (recur ancs (into [] (set (concat acc rest)))
               ))
      ))
  )

(defn count-ancestors
  [rules name]
  (count (get-ancestors rules name)))


(defn count-descendants
  "Will sum together the total counts of all descendants of the provided style of bag"
  [rules name]
  (loop [rest (contains rules name 1)  acc 0]
    (if (= (count rest) 0)
      acc
      (let [
            desc (reduce #(set (concat %1 (contains rules (:name %2) (:count %2)))) [] rest)
            desc-count (reduce #(+ %1 (:count %2)) acc rest)]
        (recur desc desc-count)
        )
      )
    )
  )

(comment
  (def line "light red bags contain 1 bright white bag, 2 muted yellow bags.")
  (def line2 "light blue bags contain no other bags")
  (count-ancestors (load-input) "shiny gold")
  (count-descendants (load-input) "shiny gold")
  )
