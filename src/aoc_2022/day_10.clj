(ns aoc-2022.day-10
  (:require [aoc-2022.core :as core]
            [clojure.string :as str]))

(defn get-inp []
  (->> "10"
       (core/get-input)
       (map #(str/split % #" "))))

(defn build-states
  "reducer to build states vector"
  [states [cmd s]]
  (let [cur-state (last states)]
    (cond (= cmd "noop") (conj states cur-state)
          (= cmd "addx") (conj states cur-state (+ cur-state (read-string s)))
          :else (throw (Exception. "upsie")))))

(defn get-states-arr-from
  "given a start state, builds next states using build-states reducer"
  [x]
  (reduce build-states x (get-inp)))

(defn get-val
  "gets the state val for a given n(with start 1 index notation)"
  [n]
  (* (nth (get-states-arr-from [1]) (dec n)) n))

(defn part-1 []
  (let [indexes (take 6 (iterate (partial + 40) 20))]
    (apply + (map get-val indexes))))

(defn part-2
  "prints the results of state array"
  []
  (loop [to-print (range 240)]
    (if (nil? to-print)
      to-print
      (let [i (first to-print)
            moded (mod i 40)]
        (cond
          (= moded 0) (print "\n")
          (get #{(dec moded) moded (inc moded)} (nth (get-states-arr-from 1) i)) (print "#")
          :else (print "."))
        (recur (next to-print))))))

(part-1)
(part-2)
