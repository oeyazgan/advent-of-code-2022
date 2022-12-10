(ns aoc-2022.day-8
  (:require [aoc-2022.core :as core]))

;; part-1
(defn get-input-day-8 []
  (let [inp (core/get-input "8")
        vecs (map #(apply vector %) inp)
        ints (for [i vecs] (map #(Character/digit % 10) i))]
    ints))

(defn transpose [m]
  (apply mapv vector m))

(defn get-max-matrix-for [inp]
  (loop [max-val -1
         max-matrix []
         max-grand-matrix []
         matrix inp
         current-matrix (first matrix)]
    (cond
      (seq current-matrix)
      (recur (apply max [(first current-matrix) max-val])
             (conj max-matrix max-val)
             max-grand-matrix
             matrix
             (next current-matrix))

      matrix
      (recur
       -1
       []
       (conj max-grand-matrix max-matrix)
       (next matrix)
       (first (rest matrix)))

      :else max-grand-matrix)))

(defn get-visibles [inp max-matrix]
  (for [i (range (count inp))] (map > (nth inp i) (nth max-matrix i))))

(defn part-1 []
  (count
   (filter
    true?
    (let [ltr (get-input-day-8)
          rtl (map reverse ltr)
          td (transpose ltr)
          bu (map reverse (transpose ltr))
          [max-ltr max-rtl max-td max-bu] (map get-max-matrix-for [ltr rtl td bu])
          [v-ltr v-rtl v-td v-bu] (map get-visibles [ltr rtl td bu] [max-ltr max-rtl max-td max-bu])]
      (map #(some true? [% %2 %3 %4])
           (flatten v-ltr)
           (flatten (map reverse v-rtl))
           (flatten (transpose v-td))
           (flatten (transpose (map reverse v-bu))))))))

;; part-2
(defn twod-lst-to-vec [twod-lst]
  (vec (map vec twod-lst)))

(defn get-right-v [inp x y]
  (loop [cur-x x v-count 0]
    (let [row (nth inp y)
          elem (nth row x)
          next-exists (> (count row) (+ cur-x 1))]
      (cond
        (not next-exists) v-count
        (> elem (nth row (inc cur-x))) (recur (inc cur-x) (inc v-count))
        :else (inc v-count)))))

;; calculate these one time
(def inp (get-input-day-8))
(def rev (->> inp (map reverse)))
(def tr (transpose inp))
(def rev-tr (->> inp transpose (map reverse)))

(defn calculate-scenic-score [inp x y]
  (let [cnt (count inp)]
    (* (get-right-v inp x y)
       (get-right-v rev (->> x (- cnt) dec) y)
       (get-right-v tr y x)
       (get-right-v rev-tr (->> y (- cnt) dec) x))))

(defn get-scenic-scores-for-all [inp]
  (let [cnt (count inp)]
    (for [i (range cnt) j (range cnt)]
      (calculate-scenic-score inp i j))))

(defn part-2 [] (apply max (get-scenic-scores-for-all inp)))

(defn day-8 [] [(part-1) (part-2)])

(day-8)
