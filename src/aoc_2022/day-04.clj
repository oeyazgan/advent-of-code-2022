(ns aoc-2022.day-04
   (:require [aoc-2022.core :as core]
             [clojure.string :as str]))

(defn overlaps? [[[e00 e01] [e10 e11]]]
  (cond
    (> e00 e10) (>= e11 e01)
    (< e00 e10) (>= e01 e11)
    :else true))

(defn overlaps-p2? [[[e00 e01] [e10 e11]]]
  (cond
    (> e00 e10) (>= e11 e00)
    (< e00 e10) (>= e01 e10)
    :else true))

(defn to-ints-part-4 [[x y]]
  (let [get-elves #(map read-string (str/split % #"-"))]
    [(get-elves x) (get-elves y)]))

(defn day-4 []
  (let [input (core/get-input "4")
        split-by-coma #(str/split % #",")
        get-with-overlap-fn #(map (comp % to-ints-part-4 split-by-coma) input)
        overlaps-p1 (get-with-overlap-fn overlaps?)
        overlaps-p2 (get-with-overlap-fn overlaps-p2?)
        get-result #(->> % (filter true?) count)
        result [(get-result overlaps-p1) (get-result overlaps-p2)]]
    result))

