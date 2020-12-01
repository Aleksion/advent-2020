(ns day.1.main
  (:require [
             clojure.java.io :as io
             ]))

(def file "src/day/1/input.txt")


(defn get-set [filename]
  (with-open [rdr (io/reader filename)]
    (doall (set (map #(Integer/parseInt %) (line-seq rdr))))))



(defn find-multiple [sum-value values]
  (let [match
        (first
          (filter
            #(contains? values (- sum-value %))
            values))
        ]
    (if (= match nil)
      nil
      (* match  (- sum-value match)))
    ))


(comment
  (find-multiple 2020 (get-set file))

  (first (filter (fn [v]
                   (let [match (find-multiple (- 2020 v) (get-set file))]
                     (do
                       (println v)
                       (if (= nil match)
                         false

                         (= 2020 (+ v match)))))
                   ) (get-set file))
         )
  )
