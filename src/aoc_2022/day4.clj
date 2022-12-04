(ns aoc-2022.day4
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))


(defn parse-pairs [pair-str]
  (-> pair-str
    (str/split #",")
    (->> (map (comp (fn [v] (map #(Integer/parseInt % 10) v))
                    #(str/split % #"-"))))))

(defn overlaps-fully? [[[start1 end1] [start2 end2]]]
  (or (<= start1 start2 end2 end1)
      (<= start2 start1 end1 end2)))

(defn overlaps? [[[start1 end1] [start2 end2]]]
  (not (or (< end1 start2)
           (< end2 start1))))

(defn solve [filename overlaps-fn]
  (->> filename
       io/resource
       slurp
       str/split-lines
       (map parse-pairs)
       (filter overlaps-fn)
       count))

(comment
  (solve "day4_ex.txt" overlaps-fully?)
  (solve "day4.txt" overlaps-fully?)

  (solve "day4_ex.txt" overlaps?)
  (solve "day4.txt" overlaps?)

  ,)
