(ns aoc-2022.day-2
  (:require [aoc-2022.core :as core]
            [clojure.string :as str]))

(defn result-convert [result]
  (-> result
      (str/replace #"[AX]" "0")
      (str/replace #"[BY]" "1")
      (str/replace #"[CZ]" "2")))

(defn to-ints [x]
  (let [splitted (str/split x #" ")]
    (map
     (fn [y] (-> y str read-string))
     splitted)))

(defn result-convert-2 [[elf me]]
  (cond (= me 0) [elf (-> elf dec (mod 3))]
        (= me 1) [elf elf]
        [= me 2] [elf (-> elf inc (mod 3))]))

(defn calculate-w [[elf me]]
  (let [result (- elf me)]
    (cond (= -1 result) 6
          (= 2 result) 6
          (= 0 result) 3
          :else 0)))

(defn calculate-p [[_ me]]
  (cond (= me 0) 1
        (= me 1) 2
        (= me 2) 3
        :else 0))

(defn day-2 []
  (let [input (core/get-input "2")
        encoded (map result-convert input)
        part-1-ints (map to-ints encoded)
        part-2-ints (map result-convert-2 part-1-ints)
        get-result #(->> %
                         (map (juxt calculate-w calculate-p))
                         flatten
                         (apply +))
        part-1-results (get-result part-1-ints)
        part-2-results (get-result part-2-ints)]
    [part-1-results part-2-results]))

(day-2)
