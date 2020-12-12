(ns day-11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))



(defn load-input
  [f]
  (->>  (slurp f)
        (str/split-lines)
        (mapv vec)
        ))

(def neighbour-pos [[-1 -1] [0 -1] [1  -1]
                    [-1  0]        [1   0]
                    [-1  1] [0  1] [1   1]])
(defn point->neighbours
  "Find the neighbours of a point 'p' in matrix m"
  [p m max-x max-y]
  (vec  (for [v neighbour-pos
              :let [[x y] (map + p v)]
              :when (and (<= 0 x max-x) (<= 0 y max-y))]
          (get-in m [y x]))))


(defn apply-rules-1
  [[x y] m max-x max-y]
  (let [neighbours (point->neighbours [x y] m max-x max-y)
        state (get-in m [y x])
        c-seats (count (filter #(= \# %) neighbours))
        ]
    (cond
      (and (= \L state) (zero? c-seats)) \#
      (and (= \# state) (>= c-seats 4)) \L
      :else state)))


(defn simulate-change
  [change-seat-fn seat-map]
  (let [max-x (count (first seat-map))
        max-y (count seat-map)
        seats (for [y (range max-y)
                    x (range max-x)
                    :when (not= \. (get-in seat-map [y x]))
                    ]
                [x y])]
    (reduce (fn [sm [x y]]
              (assoc-in sm [y x] (change-seat-fn [x y] seat-map max-x max-y)))
            seat-map
            seats)))

(defn solve-1
  [seat-map]
  (loop [seat-map seat-map
         prev nil
         runs 0]

    (cond
      (= prev seat-map) ((frequencies (flatten seat-map)) \#)
      (= 1000 runs) (str "Error running. Ended with: " (frequencies (flatten seat-map)))
      :else (recur (simulate-change apply-rules-1 seat-map) seat-map (inc runs))
      )))


;; Part 2
(defn- visible-seat? [seat-map dir x y]
  (let [start (mapv + [x y] dir)]
    (loop [[px py] start]
      (let [state (get-in seat-map [py px])]
        (cond
          (nil? state) false
          (= state \.) (recur (mapv + [px py] dir))
          (= state\#) true
          :else     false)))))


(defn- count-visible [[px py] seat-map max-x max-y]
  (let [directions (filter (fn [v]
                             (let [[x y] (map + v [px py])]
                               (and (<= 0 x max-x) (<= 0 y max-y))))
                           neighbour-pos)]
    (apply + (for [dir directions]
               (if (visible-seat? seat-map dir px py) 1 0)))))

(defn apply-rules-2
  [[x y] m max-x max-y]
  (let [state (get-in m [y x])
        c-seats (count-visible [x y] m max-x max-y)]
    (cond
      (and (= \L state) (zero? c-seats)) \#
      (and (= \# state) (>= c-seats 5)) \L
      :else state)))

(defn solve-2
  [seat-map]
  (loop [seat-map seat-map
         prev nil
         runs 0]

    (cond
      (= prev seat-map) ((frequencies (flatten seat-map)) \#)
      (= 1000 runs) (str "Error running. Ended with: " (frequencies (flatten seat-map)))
      :else (recur (simulate-change apply-rules-2 seat-map) seat-map (inc runs))
      )))

(comment
  (def test-input (load-input "test/fixtures/day_11_input.txt"))

  ;; Solve 1 test
  (solve-1 test-input)

  ;; Solve 1 innput
  (solve-1 (load-input (io/resource "day_11_input.txt")))

  ;; Solve 2 test
  (solve-2 test-input)

  ;; Solve 2 input
  (solve-2 (load-input (io/resource "day_11_input.txt")))

  )
