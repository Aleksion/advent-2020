(ns day-5
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   )
  )

;;
;; Input
;;
(defn load-input
  []
  (->
    (io/resource "day_5_input.txt")
    (slurp)
    (str/split-lines)
    ))


;;
;; Helpers
;;
(defn upper-half [v] (/ (+ v 1) 2))
(defn lower-half [v] (/ (- v 1) 2))


(defn upper-bounds
  [lower upper]
  (+ lower (upper-half (Math/abs (- upper lower)))))

(defn lower-bounds
  [lower upper]
  (+ lower (lower-half (Math/abs (- upper lower)))))


(defn keys->val
  "Will use binary space partioning to narrow down the bounds
  to a single value by iterating over the keys"
  [keys bounds bounds-keys]
  (loop [lower (:lower bounds) upper (:upper bounds) index 0]
    (let [key (utils/at-index keys index)]
      (if (= (- upper lower) 1)
        (if (= (:lower bounds-keys) key) lower upper)
        (let [new-lower (if (= (:upper bounds-keys) key) (upper-bounds lower upper) lower)
              new-upper (if (= (:lower bounds-keys) key) (lower-bounds lower upper) upper)]
          (recur new-lower new-upper (inc index)))))))


(defn keys->seat-map
  [key]
  (let [row (keys->val (subs key 0 7) {:lower 0 :upper 127} {:lower "F" :upper "B"})
        col (keys->val (subs key 7 10) {:lower 0 :upper 7} {:lower "L" :upper "R"})]
    {:row row
     :col col
     :sid (+ (* row 8) col)
     :key key}))


(defn parse-boarding-passes
  "Parse boarding passes from input"
  [input]
  (->>
    (map #(keys->seat-map %) input)
    (sort-by :sid)))

(defn get-seat
  "Will get the first unassigned seat with two assigned seats next to it"
  [bp-v]
  (let [assigned (set (map #(:sid %) bp-v))]
    (for [x (range (* 127 8))
          :let [prev? (contains? assigned (dec x))
                next? (contains? assigned (inc x))]
          :when (not (contains? assigned x))
          :when (and prev? next?)]
      x)))

(comment
  (keys->seat-map "FBFBBFFRLR")

  ;; Solve 1
  (last (parse-boarding-passes (load-input)))

  ;; Solve 2
  (get-seat (parse-boarding-passes (load-input)))
  )
