(ns aoc-2022.core
  (:require [clojure.string :as str]
            [clojure.set :as s]))

;; common
(defn get-input [day]
  (-> (slurp (apply str (concat "resources/day-" day ".txt")))
      (str/split #"\n")))

