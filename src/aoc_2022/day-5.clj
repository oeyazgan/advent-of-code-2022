(ns aoc-2022.day-5
   (:require [aoc-2022.core :as core]
             [clojure.string :as str]))

(def input-day-5
  (-> (slurp "resources/day-5.txt")
      (str/split #"\n\n")))

(def stacks
  (let
      [raw-input
       (-> input-day-5
           first
           (str/split #"\n")
           drop-last)
       stack-count (inc (quot (count (first raw-input)) 4))
       indices (take stack-count (iterate (partial + 4) 1))
       stacks
       (for [i indices]
         (map #(let [v (str (nth % i))]
                 (when-not (= v " ") v))
              raw-input))
       filtered (map #(filter some? %) stacks)
       to-map (into {} (for [i (range stack-count)] [i (nth filtered i)]))]
    {:stacks to-map :stack-count stack-count}))

(def moves
  (let [raw-input (-> input-day-5
                      second
                      (str/split #"\n"))
        mapped (map #(str/split % #" ") raw-input)
        dstr (map (fn [[_ n _ s _ d]] (map read-string [n s d])) mapped)]
    dstr))

(defn move-single [n s d stacks]
  (let [is (dec s)
        id (dec d)]
    (loop [i n stacks stacks]
      (if (zero? i)
        stacks
        (let [top-stack (first (get stacks is))
              pop-stack (rest (get stacks is))
              stacks (-> stacks
                         (assoc is pop-stack)
                         (assoc id (conj (get stacks id) top-stack)))]
          (recur (dec i) stacks))))))

(defn move-multiple [n s d stacks]
  (let [is (dec s)
        id (dec d)
        to-move (subvec (vec (get stacks is)) 0 n)
        remainder (subvec (vec (get stacks is)) n)
        existing (get stacks id)]
    (-> stacks
        (assoc is (apply list remainder))
        (assoc id (concat to-move existing)))))

(defn with-move-func [f]
  (let [stack-count (:stack-count stacks)
        stacks (loop [i 0 stacks (:stacks stacks)]
                 (if (= i (count moves))
                   stacks
                   (let [[n s d] (nth moves i)
                         stacks (f n s d stacks)]
                     (recur (inc i) stacks))))
        result (for [i (range stack-count)]
                 (apply str (first (get stacks i))))]
    result))

(defn day-5 []
  [(apply str (with-move-func move-single))
   (apply str (with-move-func move-multiple))])

(day-5)


