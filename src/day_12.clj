(ns day-12
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]))


(defn load-input
  [f]
  (->> (slurp f)
       (str/split-lines)
       (vec)))


(defn line->instruction
  [line]
  (let [[inst value] (re-seq #"\d+|(?:\D+)" line)]
    {:inst (.charAt inst 0)
     :value (read-string value)}))


(defn get-value
  [dir v]
  (case dir
    0 v
    90 v
    180 (- v)
    270 (- v)))


(defn do-turn
  [dir radii]
  (let [v (+ dir radii)]
    (cond
      (>= v  360) (- v 360)
      (< v 0) (+ v 360)
      :else v)))


(def coord-map {0   :y
                90  :x
                180 :y
                270 :x})



(defn solve-1
  [input]
  (loop [i 0
         dir 90
         pos {:x 0 :y 0}]
    (cond
      (= i (count input)) (+ (Math/abs (:x pos)) (Math/abs (:y pos)))
      :else
      (let [{:keys [inst value]} (line->instruction  (input i))]
        (case inst
          \F (recur (inc i) dir (update pos (coord-map dir) + (get-value dir value)))
          \E (recur (inc i) dir (update pos :x + value))
          \W (recur (inc i) dir (update pos :x + (- value)))
          \N (recur (inc i) dir (update pos :y + value ))
          \S (recur (inc i) dir (update pos :y + (- value)))
          \R (recur (inc i) (do-turn dir value) pos)
          \L (recur (inc i) (do-turn dir (- value)) pos))))))

(map * [10 10] [2 3])
(vec  (vals  {:x 1 :y 1}))

(defn rotate-waypoint
  [{:keys [x y]} radii]
  (case radii
    0 {:x x :y y}
    90 {:x y :y (- x)}
    180 {:x (- x) :y (- y) }
    270 {:x (- y) :y x}

    -90 {:x (- y) :y x}
    -180 {:x (- x) :y (- y) }
    -270 {:x y :y (- x)}))

(defn solve-2
  [input]
  (loop [i 0
         pos {:x 0 :y 0}
         waypoint {:x 10 :y 1}]
    (do (println pos waypoint)

        (cond
          (= i (count input)) (+ (Math/abs (:x pos)) (Math/abs (:y pos)))
          :else
          (let [{:keys [inst value]} (line->instruction  (input i))
                {:keys  [wx wy]} waypoint
                {:keys [px py]} pos]
            (case inst
              \F (let [[px py] (mapv #(* % value) (vals waypoint))]
                   (recur (inc i) {:x (+ px (:x pos)) :y (+ py (:y pos))}  waypoint))
              \E (recur (inc i) pos (update waypoint :x + value))
              \W (recur (inc i) pos (update waypoint :x + (- value)))
              \N (recur (inc i) pos (update waypoint :y + value ))
              \S (recur (inc i) pos (update waypoint :y + (- value)))
              \R (recur (inc i) pos (rotate-waypoint waypoint value))
              \L (recur (inc i) pos (rotate-waypoint waypoint (- value)))))))))

(comment
  (def test-file "test/fixtures/day_12_input.txt")

  ;; Solve 1 test
  (solve-1
    (load-input test-file))

  ;; Solve 1 input
  (solve-1 (load-input (io/resource "day_12_input.txt")))

  ;; Solve 2 test
  (solve-2 (load-input test-file))

  ;; Solve 2 input
  (solve-2 (load-input (io/resource "day_12_input.txt")))


  )
