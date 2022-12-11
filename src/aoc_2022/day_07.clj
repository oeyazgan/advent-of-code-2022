(ns aoc-2022.day-07
  (:require [aoc-2022.core :as core]
            [clojure.string :as str]))

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
  (loop [inp (core/get-input "7") cur-path "" directories {}]
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
