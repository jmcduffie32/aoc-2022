(ns aoc-2022.day2
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))


(defn shape-value [shape]
  (case shape
    ("A" "X") 1
    ("B" "Y") 2
    ("C" "Z") 3))

(defn parse-rounds [data-str]
  (->> data-str
      str/split-lines
      (mapv #(str/split % #"\s"))))

(defn score-round [round]
  (let [[op you :as throw-values] (mapv shape-value round)]
    (cond
      ;; draw
      (apply = throw-values) (+ 3 you)
      ;; loss
      (and (= op 1) (= you 3)) you
      (and (= op 3) (= you 1)) (+ 6 you)
      ;; win
      (> you op) (+ 6 you)
      :else you)))

(defn choose-shape [[op outcome]]
  (cond
    ;; X loose
    (= outcome "X")
    (case op
      "A" "C"
      "B" "A"
      "C" "B")
    ;; Y draw
    (= outcome "Y") op
    ;; Z win
    (= outcome "Z")
    (case op
      "A" "B"
      "B" "C"
      "C" "A")))

(comment
  (def example-data
    "A Y\nB X\nC Z")

  ;; part 1
  (def data (slurp (io/resource "day2.txt")))
  (->> data
       parse-rounds
       (mapv score-round)
       (apply +))

  ;; part 2
  (->> data
       parse-rounds
       (mapv (fn [[op :as round]] [op (choose-shape round)]))
       (mapv score-round)
       (apply +))
  ,)
