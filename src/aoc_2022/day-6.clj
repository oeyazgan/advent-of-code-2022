(ns aoc-2022.day-6)

(defn with-partition [prt]
  (->> (slurp "resources/day-6.txt")
       (partition prt 1)
       (map (comp count #(apply hash-set %)))
       (map-indexed vector)
       (drop-while (fn [[x y]] (not= y prt)))
       (#((comp (partial + prt) first first) %))))

(defn day-6 []
  [(with-partition 4) (with-partition 14)])

(day-6)
