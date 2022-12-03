(ns aoc-2022.core
  (:require [clojure.string :as str]
            [clojure.set :as s]))

;; common
(defn get-input [day]
  (-> (slurp (apply str (concat "resources/day-" day ".txt")))
      (str/split #"\n")))

;; day 1
(defn day-1 []
  (let [input (get-input "1")
        groups (partition-by #(= 0 (count %)) input)
        elves (filter #(not= % '("")) groups)
        elves-int (map #(map read-string %) elves)
        sums (map #(apply + %) elves-int)
        sorted-sums (sort > sums)]
  [(first sorted-sums) (apply + (take 3 sorted-sums))]))

;; day 2
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
  (let [input (get-input "2")
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

;; day 3
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
  (let [input (get-input "3")
        priorities (map get-priority input)
        result (apply + priorities)]
    result))

(defn day-3-part-2 []
  (let [input (get-input "3")
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
