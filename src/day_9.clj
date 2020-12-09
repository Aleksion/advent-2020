(ns day-9
  (:require [clojure.string :as str]
            [utils :refer [queue]]

            [clojure.java.io :as io]))


(defn load-input
  []
  (->
    (io/resource "day_9_input.txt")
    (slurp)))

(defn input->numbers [input]
  (->>
    (str/split-lines input)
    (map #(read-string %))
    (into [])))


(defn sum->addends
  [target numbers]
  (for [x numbers
        y numbers
        :let [is-sum? (= target (+ x y))]
        :when (not= x y)
        :when (true? is-sum?)] x))



(defn find-non-addend
  "Will find the first number that isn't a sum of the 'n' numbers preceding it"
  [n numbers]
  (let [que []]
    (loop [que (queue)
           i 0]
      (let [number (numbers i)
            is-sum? (>=  (count (sum->addends number que)) 2)
            ]
        (cond
          (>= i (count numbers)) "Smooth Run"
          (< i n) (recur (conj que number) (inc i))
          ;; Test the values
          (false? is-sum?) number
          :else (recur (->
                         (conj que number)
                         (pop))
                       (inc i)))))))



(defn find-sum-range
  "Tests whether a contiguous set of values
  from index 0 sums to the provided target"
  [target numbers]
  (first  (for [i (range (count numbers))
                :let [values (subvec numbers 0 i)
                      sum (apply + values)]
                :when (= sum target)
                :while (<= target sum)
                                        ;:while (< sum target)
                ]

            values)))


(defn find-encryption-weakness
  "Will find the encryption weakness by testing which contigous
  values sum up to the first 'non-addedn' value"
  [n numbers]
  (let [target (find-non-addend n numbers)]
    (loop [i 0]
      (let [range (find-sum-range target (subvec numbers i))
            is-weakness? (vector? range)
            ]
        (cond
          is-weakness? (-> (sort range)
                           (#(+ (first %) (last %)))
                           )
          (< i (count numbers)) (recur (inc i))
          :else "No weakness! :O")))))




(comment
  (def input "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")


  ;; Solve Test
  (->>
    (input->numbers input)
    (find-non-addend 5)
    )
  ;; Solve 1
  (->>
    (load-input)
    (input->numbers)
    (find-non-addend 25)
    (println))

  ;; Solve Test 2
  (->>
    input
    (input->numbers)
    (find-encryption-weakness 5)
    (println))

  ;; Solve 2
  (->>
    (load-input)
    (input->numbers)
    (find-encryption-weakness 25)
    (println))


  )
