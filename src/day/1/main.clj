(ns day.1.main
  (:require [
             clojure.java.io :as io
             ]))

(defn lines [filename]
  (with-open [rdr (io/reader filename)]
    (doall (line-seq rdr))))


(comment
  (lines "input.txt")
  )
