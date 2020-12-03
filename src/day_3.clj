(ns day-3
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   ))


;; Test inputs
(def file "day_3_input.txt")


(defn load-input
  [f]
  (->>
    (io/resource f)
    (slurp)))

(def test-1-path [3 1])
(def test-2-paths
  "Slope paths"
  [[1 1] [3 1] [5 1] [7 1] [1 2]])



;; Test Solutions
(defn has-tree?
  "Will check for trees on the provided position and row"
  [row pos]
  (let [total (count row)
        new-pos (if (>= pos total)
                  (mod pos total)
                  pos)]
    (= "#"  (subs row new-pos (inc new-pos)))))

(defn n-of-trees-on-path
  "Returns the number of trees on a given grid 'g'
  given a traversing path 'P"
  [g p]
  (let [[x-steps y-steps] p
        rows (str/split-lines g)]
    (loop [x 0 y 0 trees 0]
      (if (>= y (count rows))
        trees
        (recur (+ x x-steps) (+ y y-steps)
               (if (has-tree? (nth rows y) x)
                 (inc trees)
                 trees))))))

(defn check-trees-on-slopes
  "Returns the product of the count of trees on each path in the vector v-p
  through the grid 'g'"
  [g v-p]
  (->>
    (map #(n-of-trees-on-path g %) v-p)
    (apply *)))


(comment
  (n-of-trees-on-path (load-input file) test-1-path)
  (println  (check-trees-on-slopes (load-input file) test-2-paths)))
