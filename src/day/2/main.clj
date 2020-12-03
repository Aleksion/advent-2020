(ns day.2.main
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.core.match :refer [match]]
   )
  )


(def file "day_2_input.txt")


(defn count-char
  "Count the occurences of char 'c' in string 's'"
  [c s]
  (->
    (re-seq (re-pattern c) s)
    count
    ))


(defn parse-input
  "Will read the input file
  and parse the password sequences into a map
  for further processing
  "
  [f]
  (->>
    (io/resource f)
    (io/reader)
    (line-seq)
    (map (fn [v] (let [
                      [band v pwd](str/split v #" ")
                      [x y] (map #(Integer/parseInt %) (str/split band #"-"))
                      v (str/replace v ":" "")
                      ]
                  {:x x
                   :y y
                   :value v
                   :pwd pwd
                   })
           )))
  )


(defn check-first-policy
  "Given a map containing {:x :y :value :pwd} where :x and :y
  describes min and max occurences of v in pwd
  return true if the pwd is valid or false if it isn't"
  [m]
  (let [
        min (:x m)
        max (:y m)
        c (count-char (:value m) (:pwd m))
        ]
    (and (>= c min) (<= c max))
    )
  )

(defn check-second-policy
  "Given a map containing {:x :y :value :pwd} where :x and :y
  describes the first location and second location of :value
  return true if the pwd is valid or false if it isn't"
  [m]
  (let [
        v1 (:value m)
        v2 (:value m)
        xv (subs (:pwd m) (dec (:x m)) (:x m))
        yv (subs (:pwd m) (dec (:y m)) (:y m))
        ]
                                         ; Couldn't get it to match on v in both places
                                        ; so I had to split up v into two local bindings (v1 and v2)
    (match [xv yv]

      [v1 v2] false
      [v1 _] true
      [_ v2] true
      :else false
      )
    ))



(defn solve-1
  [f]
  (->>
    (parse-input f)
    (filter #(check-first-policy %))
    count
    )
  )

(defn solve-2
  [f]
  (->>
    (parse-input f)
    (filter #(check-second-policy %))
    count
    )
  )

(comment
  (solve-1 file)
  (solve-2 file)
  )
