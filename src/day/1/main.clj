(ns day.1.main
  (:require [
             clojure.java.io :as io
             ]))


(def file "src/day/1/input.txt")


(defn get-set [filename]
  (with-open [rdr (io/reader filename)]
    (doall (set (map #(Integer/parseInt %) (line-seq rdr))))))



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
  [total values times]
  (if (<= times 2)
    (find-sum-match total values)
    (some (fn [vs]
            (let [
                  new-total (- total vs)
                  v (find-sum-value new-total values)
                  ]
              (if (= v nil)
                false
                (let [res (conj (find-sum-recur new-total values (- times 1)) vs)]
                  (if (= (count res) times)
                    res
                    false)))
              )
            ) values)

    )
  )


(defn find-multiple
  [total  values times]
  (reduce #(* %1 %2) (find-sum-recur total values times))
  )

(comment
  (println  (find-multiple 2020 (get-set file) 3))
  (println  (find-multiple 2020 (get-set file) 2))
  )
