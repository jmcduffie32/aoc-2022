(ns aoc-2022.day13
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def example
  "[[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]

[[2]]
[[6]]]")

(defn cmp [l r]
  (cond
    (and (number? l) (number? r))
    (<= l r)
    (number? r)
    (recur l [r])
    (number? l)
    (recur [l] r)
    :else
    (cond (empty? l)                      true
          (empty? r)                      false
          (not (cmp (first l) (first r))) false
          (cmp (first r) (first l))       (cmp (rest l) (rest r))
          :else                           true)))

(defn solve [str]
  (->> (eval (read-string str))
       (partition 2)
       (map (partial apply cmp))
       (map-indexed (fn [i v] (if v (inc i) 0)))
       (apply +)))

(comment
  (solve (slurp (io/resource "day13.txt")))

  (->> (eval (read-string (slurp (io/resource "day13.txt"))))
       (sort (fn [l r] (not (cmp r l))))
       (map-indexed (fn [i e] (if (or (= e [[2]])
                                      (= e [[6]]))
                                (inc i)
                                1)))
       (apply *))

  ,)
