(ns day-10
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input-file (io/resource  "day_10_input.txt"))

(defn load-input
  [file]
  (->> (slurp file)
       (str/split-lines)
       (map #(read-string %))
       (concat [0])
       (sort)
       (into [])))



(defn get-diff
  [adapters i]
  (if (= 0 i)
    (adapters i)
    (- (adapters i) (adapters (dec i)))))

(defn adapters->jolt-differences
  [adapters]
  (loop [i 0
         res {:1 0 :2 0 :3 0}]
    (cond
      (= i (count adapters)) (update res :3 inc)
      (= 3 (get-diff adapters i)) (recur (inc i) (update res :3 inc))
      (= 2 (get-diff adapters i)) (recur (inc i) (update res :2 inc))
      (= 1 (get-diff adapters i)) (recur (inc i) (update res :1 inc))
      :else (str "Diff was " (get-diff adapters i) " at element " i))))


(defn solve-1
  [adapters]
  (let [res (adapters->jolt-differences adapters)]
    (* (:1 res) (:3 res))))



(defn solve-2
  [adapters]
  ;; Reverse the adapters and walk back
  (let [adapters (vec  (reverse  adapters))]
    ;; We want to start from i 1, so the first iteration can pull
    ;; a value on (+ paths 1/2/3)
    (loop [i 1
           results {(first adapters) 1}]
      (cond
        (>= i (count adapters)) (results 0)
        :else (recur (inc i) (let [paths (adapters i)]
                               (assoc results
                                      paths
                                      (apply + [(get results (+ paths 1) 0)
                                                (get results (+ paths 2) 0)
                                                (get results (+ paths 3) 0)]))))))))


(comment
  (def test-input (load-input  "test/fixtures/day_10_input.txt"))
  (def test-input-1 (load-input  "test/fixtures/day_10_input_1.txt"))

  ;; Solve Test 1
  (solve-1 test-input)

  ;; Solve 1
  (solve-1
    (load-input input-file))

  ;; Solve 2
  (println test-input-1)
  (time (solve-2 (load-input input-file)))

  )
