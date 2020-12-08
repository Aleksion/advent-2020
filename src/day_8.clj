(ns day-8
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))




;; Inpyt
(defn load-input []
  (->
    (io/resource "day_8_input.txt")
    (slurp)
    ))


;; Logic
(defn line->instruction
  "Convert a line to an instruciton set"
  [line n]
  (let [[cmd arg](str/split line #" ")]
    {:cmd cmd :arg (Integer/parseInt arg) :line n}))

(defn input->instructions
  [input]
  (->>
    (str/split-lines input)
    (map-indexed #(line->instruction %2 %1))
    (into [])
    )
  )

(defn exec-instruction
  "Will exec the specified instruction set and return and updated
  accumulator and the next line tobe executed number 'n'"
  [instruction acc]
  (let [{:keys [cmd arg line]} instruction]
    (case cmd
      "nop" {:acc acc :next (inc line)}
      "acc" {:acc (+ acc arg) :next (inc line)}
      "jmp" {:acc acc :next (+ line arg)})))


(defn swap-cmd
  "Will swap from a nop and jmp or vice versa"
  [inst]
  (let [new-cmd (if (=  (:cmd inst) "jmp") "nop" "jmp")]
    (assoc inst :cmd new-cmd)
    ))

(defn exec-prg
  "Will exec the provided program and return
  the accumulator on the first line that is visited twice or at
  the end of the program"
  [instructions]
  (loop [lines #{} n 0 acc 0]
    (cond
      (contains? lines n) {:acc acc :broken? true}
      (> n (dec (count instructions))) {:acc acc :broken? false}
      :else
      (let [{:keys [acc next]} (exec-instruction (instructions n) acc)]
        (recur (conj lines n) next acc)))))


(defn exec-broken-prg
  "Will attempt fixing every nop and jmp operation until it hits a working program"
  [program]
  (loop [tested-ops #{}
         next-program program
         ]
    (let [{:keys [broken? acc]} (exec-prg next-program)
          suspect-ops (filter (fn [v] (and (false? (contains? tested-ops v))
                                          (some #(= (:cmd v) %) ["nop" "jmp"])))
                              program)
          next-op (first suspect-ops)]
      (cond
        (empty? suspect-ops) (throw (Exception. "Did not find result"))
        (false? broken?) acc
        (true? broken?) (recur (conj tested-ops next-op)
                               (assoc program (:line next-op) (swap-cmd next-op)))
        :else nil))))



(comment
  (def input "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

  (exec-prg (input->instructions input))
  (exec-prg (input->instructions  (load-input)))

  ;; Solve 2
  (->>
    (input->instructions (load-input))
    (exec-broken-prg)
    )


  )
