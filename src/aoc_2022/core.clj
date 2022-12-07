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

;; day-4
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
  (let [input (get-input "4")
        split-by-coma #(str/split % #",")
        get-with-overlap-fn #(map (comp % to-ints-part-4 split-by-coma) input)
        overlaps-p1 (get-with-overlap-fn overlaps?)
        overlaps-p2 (get-with-overlap-fn overlaps-p2?)
        get-result #(->> % (filter true?) count)
        result [(get-result overlaps-p1) (get-result overlaps-p2)]]
    result))

;; day 5
;; I am not proud of this code
;; really not

;; please dont look
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

;; day-6
(defn with-partition [prt]
  (->> (slurp "resources/day-6.txt")
       (partition prt 1)
       (map (comp count #(apply hash-set %)))
       (map-indexed vector)
       (drop-while (fn [[x y]] (not= y prt)))
       (#((comp (partial + prt) first first) %))))

(defn day-6 []
  [(with-partition 4) (with-partition 14)])

;; day-7
;; there're some issues with this code... but it works and I want to keep pushing every day
(defmulti parse-by
  (fn [x] (subs (:command x) 0 4)))

(defmethod parse-by "$ cd" [{:keys [command cur-path directories]}]
  (let [[_ _ pth] (str/split command #" ")]
    (cond (= pth "..") {:cur-path (subs cur-path 0 (str/last-index-of cur-path "/"))
                        :directories directories}
          (= pth "/") {:cur-path ""
                       :directories directories}
          :else {:cur-path (apply str (concat cur-path "/" pth))
                  :directories directories})))

(defmethod parse-by "dir " [{:keys [command cur-path directories]}]
  (let [folder (-> command (str/split #" ") second)
        new-full-path (apply str (concat cur-path "/" folder))
        with-new-dir (assoc directories new-full-path [])
        update-parent-dir (assoc with-new-dir cur-path (conj (get directories cur-path) new-full-path))]
    {:cur-path cur-path
     :directories update-parent-dir}))

(defmethod parse-by "$ ls" [x] (dissoc x :command))

(defmethod parse-by :default [{:keys [command cur-path directories]}]
  (let [size (-> command
                 (str/split #" ")
                 first
                 read-string)]
    {:cur-path cur-path :directories (assoc directories cur-path (conj (get directories cur-path) size))}))

(defn get-directories []
  (loop [inp (get-input "7") cur-path "" directories {}]
    (if inp
      (let [command (first inp)
            parsed (parse-by {:command command
                              :cur-path cur-path
                              :directories directories})]
        (recur (next inp) (:cur-path parsed) (:directories parsed)))
      directories)))

(defn calculate-size [path directories]
  (loop [dir-vals (get directories path) size 0]
    (if dir-vals
      (let [frst (first dir-vals)]
        (if (= (type frst) java.lang.String)
          (recur (next dir-vals) (+ size (second (calculate-size frst directories))))
          (recur (next dir-vals) (+ size frst))))
      [path size])))

(defn day-7 []
  (let [sizes (map #(calculate-size % (get-directories)) (keys (get-directories)))
        less-than-100k (filter (partial > 100000) (map second sizes))
        result-p1 (apply + less-than-100k)
        to-map (apply hash-map (flatten sizes))
        total-size (get to-map "")
        to-empty-size (- total-size 40000000)
        diffs (map (partial - to-empty-size) (map second sizes))
        negatives (filter neg? diffs)
        result-p2 (-> negatives sort last (* -1) (+ to-empty-size))]
    [result-p1 result-p2]))

(day-7)
