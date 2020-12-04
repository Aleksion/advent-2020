(ns day-4
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [malli.core :as m]
   [malli.transform :as mt]))


;;
;; Inputs
;;
(defn load-input
  []
  (->
    (io/resource "day_4_input.txt")
    (slurp)))




;;
;; Logic
;;

(defn parse-passport
  "Creates a passport map given a vector 'v' of stringified passport keys"
  [v schema]
  (let [m (->>
            (str/split v #"\s+")
            (map  #(str/split % #":"))
            (into {}))]
    (m/decode schema m mt/string-transformer)))



(def passport-schema-1
  [:map
   ["byr" int?]
   ["iyr" int?]
   ["eyr" int?]
   ["hgt" string?]
   ["hcl" string?]
   ["ecl" string?]
   ["pid" string?]
   ["cid" {:optional true} string?]
   ])

(def passport-schema-2
  [:map
   ["byr" [:and int? [:>= 1920] [:<= 2002]]]
   ["iyr" [:and int? [:>= 2010] [:<= 2020]]]
   ["eyr" [:and int? [:>= 2020] [:<= 2030]]]
   ["hgt" [:and string? [:re #"(?:1[5-8][0-9]|19[0-3])+cm|(?:59|6[0-9]|7[0-6])+in"]]]
   ["hcl" [:and string? [:re #"\#[0-9a-f]{6}"]]]
   ["ecl" [:and string? [:enum "amb" "blu" "brn" "gry" "hzl" "grn" "oth"]]]
   ["pid" [:and  string? [:re #"^[0-9]{9}$"]]]
   ["cid" {:optional true} string?]])

(defn validate-passport
  "Validates a passport given an array of required keys"
  [passport schema]
  (m/validate schema passport))



(defn count-valid-passports
  ""
  [input schema]
  (->>
    (str/split input #"\n\n")
    (map #(parse-passport % schema))
    (filter #(validate-passport % schema))
    count))


(comment
  (count-valid-passports (load-input) passport-schema-1)
  (count-valid-passports (load-input) passport-schema-2))
