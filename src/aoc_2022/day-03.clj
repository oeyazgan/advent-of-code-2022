(ns aoc-2022.day-03
   (:require [aoc-2022.core :as core]
             [clojure.set :as s]))

(defn convert-priority [x]
  (let [int-val (int x)]
    (if (<= int-val 96)
      (- int-val 38)
      (- int-val 96))))

(defn get-priority [rucksack]
  (let [half-len (-> rucksack count (/ 2))
        get-halves #(->> rucksack (% half-len) set)
        first-half (get-halves take)
        second-half (get-halves drop)
        mistake (s/intersection first-half second-half)
        priority (-> mistake first char convert-priority)]
    priority))

(defn day-3-part-1 []
  (let [input (core/get-input "3")
        priorities (map get-priority input)
        result (apply + priorities)]
    result))

(defn day-3-part-2 []
  (let [input (core/get-input "3")
        groups (partition 3 input)
        sets (map #(map set %) groups)
        get-badge #(apply s/intersection %)
        badges (map get-badge sets)
        get-priority-p2 #(-> % first char convert-priority)
        priorities (map get-priority-p2 badges)
        result (apply + priorities)]
    result))

(defn day-3 []
  [(day-3-part-1) (day-3-part-2)])

(day-3)
