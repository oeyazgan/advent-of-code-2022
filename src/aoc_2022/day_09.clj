(ns aoc-2022.day-09
  (:require [aoc-2022.core :as core]
            [clojure.string :as str]))

;; day-9
(defn input-beautifier
  "from U 4 to U U U U"
  [input-row]
  (let [parsed (-> input-row (str/split #" "))
        to-int (-> parsed second read-string)
        build-seq (repeat to-int (first parsed))]
    build-seq))

(defn get-beautiful-input []
  (->> (core/get-input "9") (map input-beautifier) flatten))

(defn get-head-new-pos [[hox hoy] command]
  (let [[hnx hny]
        (condp = command
          "R" [(inc hox) hoy]
          "L" [(dec hox) hoy]
          "U" [hox (inc hoy)]
          "D" [hox (dec hoy)])]
    [hnx hny]))

(defn get-tail-new-pos [[hnx hny] [tox toy]]
  (let [xdiff     (- hnx tox)
        ydiff     (- hny toy)
        xdiff-abs (Math/abs xdiff)
        ydiff-abs (Math/abs ydiff)]
    (cond (and (< xdiff-abs 2) (< ydiff-abs 2)) [tox toy]
          (and (> xdiff 0) (> ydiff 0))         [(inc tox) (inc toy)]
          (and (< xdiff 0) (> ydiff 0))         [(dec tox) (inc toy)]
          (and (< xdiff 0) (< ydiff 0))         [(dec tox) (dec toy)]
          (and (> xdiff 0) (< ydiff 0))         [(inc tox) (dec toy)]
          (= 2 xdiff)                           [(inc tox) toy]
          (= -2 xdiff)                          [(dec tox) toy]
          (= 2 ydiff)                           [tox (inc toy)]
          (= -2 ydiff)                          [tox (dec toy)]
          :else                                 (throw (Exception. "oh gosh")))))

(defn move-knots
  "given a head and the knots,
   move each knot in head-tail pairs"
  [head knots]
  (let [reducer (fn [all n]
                  (into all [(get-tail-new-pos (last all) n)]))]
    (reduce reducer head knots)))

(defn get-visited [knot]
  (loop [inp (get-beautiful-input)
         hp [0 0]
         tails (vec (repeat knot [0 0]))
         visited #{}]
    (if inp
      (let [command (first inp)
            new-head (get-head-new-pos hp command)
            new-knots (rest (move-knots [new-head] tails))
            new-visited (conj visited (last new-knots))]
        (recur (next inp) new-head new-knots new-visited))
      visited)))

(defn day-9 []
  [(count (get-visited 1)) (count (get-visited 9))])

(day-9)
