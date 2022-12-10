(ns aoc-2022.day-1
  (:require [aoc-2022.core :as core]))

(defn day-1 []
  (let [input (core/get-input "1")
        groups (partition-by #(= 0 (count %)) input)
        elves (filter #(not= % '("")) groups)
        elves-int (map #(map read-string %) elves)
        sums (map #(apply + %) elves-int)
        sorted-sums (sort > sums)]
    [(first sorted-sums) (apply + (take 3 sorted-sums))]))

(day-1)

