(ns aoc-2022.core
  (:require [clojure.string :as str]))

(defn get-input [day]
  (-> (slurp (apply str (concat "resources/day-" day ".txt")))
      (str/split #"\n")))

(defn day-1 []
  (let [input (get-input "1")
        groups (partition-by #(= 0 (count %)) input)
        elves (filter #(not= % '("")) groups)
        elves-int (map #(map read-string %) elves)
        sums (map #(apply + %) elves-int)
        sorted-sums (sort > sums)]
  [(first sorted-sums) (apply + (take 3 sorted-sums))]))

(day-1)
