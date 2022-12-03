(ns aoc-2022.clj
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn- char->priority [ch]
  (let [i (int ch)]
    (if (>= i (int \a))
      (- i  96)
      (- i 38))))

(defn solve1 [data-str]
  (->> data-str
       str/split-lines
       (map #(split-at (/ (count %) 2) %))
       (map (fn [[pack1 pack2]]
              (filter #(contains? (into #{} pack2) %)
                      pack1)))
       (map (comp char->priority first))
       (apply +)))

(defn- find-badge [[pack1 pack2 pack3]]
  (let [pack2-set (into #{} pack2)
        pack3-set (into #{} pack3)]
    (filter #(and (contains? pack2-set %)
                  (contains? pack3-set %))
            pack1)))

(defn solve2 [data-str]
  (->> data-str
       str/split-lines
       (partition 3)
       (map find-badge)
       (map (comp char->priority first))
       (apply +)))

(comment
  (-> "day3.txt"
      io/resource
      slurp
      solve1)

  (-> "day3.txt"
      io/resource
      slurp
      solve2)
  ,)
