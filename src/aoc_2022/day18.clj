(ns aoc-2022.day18
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def example
  "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5")

(def point-list
  (->> (slurp (io/resource "day18.txt"))
       str/trim-newline
       str/split-lines
       (map #(map (fn [s] (Integer/parseInt s 10))
                  (str/split % #",")))
       (into #{})))

(defn point-in-list? [point]
  (contains? point-list point))

(def list-check (memoize point-in-list?))

(defn clear-sides [point]
  (let [[x y z] point
        points-to-check [(list (inc x) y z)
                         (list (dec x) y z)
                         (list x (inc y) z)
                         (list x (dec y) z)
                         (list x y (inc z))
                         (list x y (dec z))]]
    (count (filter (comp not list-check) points-to-check))))

(def steam-list (atom #{(list 0 0 0)}))

(defn sides-touching-steam [point]
  (let [[x y z] point
        points-to-check [(list (inc x) y z)
                         (list (dec x) y z)
                         (list x (inc y) z)
                         (list x (dec y) z)
                         (list x y (inc z))
                         (list x y (dec z))]]
    (count (filter @steam-list points-to-check))))

(defn steam? [point]
  (let [[x y z] point
        points-to-check [(list (inc x) y z)
                         (list (dec x) y z)
                         (list x (inc y) z)
                         (list x (dec y) z)
                         (list x y (inc z))
                         (list x y (dec z))]]
    (and (some @steam-list points-to-check)
         (not (list-check point)))))

(comment
  (clear-sides (list 1 1 1))
  (point-in-list? (list 0 0 0))
  (->> point-list
       (map clear-sides)
       (apply +))

  (->> point-list
       (map sides-touching-steam)
       (apply +))


  (doseq [p (for [x (range -1 23)
                  y (range -1 23)
                  z (range -1 23)]
              (list x y z))]
    (when (steam? p)
      (swap! steam-list conj p)))

  (count @steam-list)
  (count point-list)

  )
