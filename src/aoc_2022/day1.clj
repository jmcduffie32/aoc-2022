(ns aoc-2022.day1
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def example-data
  "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(defn elf-totals [str-data]
  (as-> str-data data
    (str/split data #"\n\n")
    (map str/split-lines data)
    (map #(apply + (map (fn [v] (Integer/parseInt v)) %)) data)))

(defn solve-max-elf [str-data]
  (->> str-data
       elf-totals
       (apply max)))

(defn solve-top-3-elves [str-data]
  (->> str-data
       elf-totals
       (sort >)
       (take 3)
       (apply +)))

(comment
  (solve-max-elf example-data)

  (solve-max-elf (slurp (io/resource "day1_part1.txt")))

  (solve-top-3-elves (slurp (io/resource "day1_part1.txt")))
  ,)
