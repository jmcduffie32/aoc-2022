(ns aoc-2022.day6
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(defn find-marker [str length]
  (loop [[head & rest] (partition length 1 str)
         n 0]
    (if (= (count (into #{} head))
           (count head))
      (+ length n)
      (recur rest (inc n)))))
