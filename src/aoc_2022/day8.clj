(ns aoc-2022.day8
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(defn mark-visibility [row]
  (loop [height -1
         i 0
         rowv (into [] row)]
    (if (= i (count rowv))
      rowv
      (recur (if (< height (:height (get rowv i)))
               (:height (get rowv i))
               height)
             (inc i)
             (if (< height (:height (get rowv i)))
               (assoc-in rowv [i :visible] true)
               rowv)))))

(defn count-visible-trees [el heights]
  (loop [[head & rest] heights
         score 0]
    (if (nil? head)
      score
      (if (>= head (:height el))
        (inc score)
        (recur rest
               (inc score))))))

(defn score-visibility [row]
  (loop [i 0
         heights '()
         rowv (into [] row)]
    (if (= i (count rowv))
      rowv
      (let [height (get-in rowv [i :height])
            score (if (= i 0)
                    0
                    (count-visible-trees (get rowv i) heights))]
        (recur (inc i)
               (conj heights height)
               (update-in rowv [i :scores] conj score))))))

(defn parse-heights [s]
  (mapv (fn [height-str]
          {:height (Integer/parseInt height-str 10)
           :visible false})
        (str/split s #"")))

(defn apply-from-all-directions [f grid]
  (->> grid
   (mapv f)
   (mapv reverse)
   (mapv f)
   (apply mapv list)
   (mapv f)
   (mapv reverse)
   (mapv f)))

(defn solve1 []
  (->>
   (slurp (io/resource "day8.txt"))
   str/split-lines
   (mapv parse-heights)
   (apply-from-all-directions mark-visibility)
   (apply concat)
   (filter :visible)
   count))

(defn solve2 []
  (->> (slurp (io/resource "day8.txt"))
       str/split-lines
       (mapv parse-heights)
       (apply-from-all-directions score-visibility)
       (apply concat)
       (mapv #(apply * (:scores %)))
       (apply max)))
