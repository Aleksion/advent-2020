(ns day-1
  (:require [
             clojure.java.io :as io
             ]))


(def file "day_1_input.txt")


(defn get-set [filename]
  (->>
    (io/resource file)
    (io/reader)
    (line-seq)
    (map #(Integer/parseInt %))
    (set)
    (doall)
    ))




(defn find-sum-value
  "Find the first value that, when multiplied with another adds up to the total"
  [total values]
  (some #(and (contains? values (- total %))  %)
        values)
  )


(defn find-sum-match
  "Return the first two values that adds up to the total"
  [total values]
  (let [x (find-sum-value total values)]
    (if (= nil x)
      nil
      [x (- total x)])
    ))

(defn find-sum-recur
  "Recursively find the values  that adds up to the provided total"
  [total values n]
  (if (<= n 2)
    (find-sum-match total values)
    (some (fn [vs]
            (let [
                  new-total (- total vs)
                  v (find-sum-value new-total values)
                  ]
              (if (= v nil)
                false
                (let [res (conj (find-sum-recur new-total values (- n 1)) vs)]
                  (if (= (count res) n)
                    res
                    false)))
              )
            ) values)

    )
  )


(defn find-multiple
  [total  values n]
  (reduce #(* %1 %2) (find-sum-recur total values n))
  )

(comment
  (println  (find-multiple 2020 (get-set file) 3))
  (println  (find-multiple 2020 (get-set file) 2))
  )
