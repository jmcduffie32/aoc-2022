(ns aoc-2022.day14
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def example
  "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(def grid
  (into []
        (for [y (range 170)]
          (into [] (for [x (range 1000)]
                     {:x x :y y :state "."})))))

(defn create-line [[[x1 y1] [x2 y2]]]
  (->> (for [x (range (min x1 x2) (inc (max x1 x2)))
             y (range (min y1 y2) (inc (max y1 y2)))]
         {:x x :y y})))

(defn str->points [str]
  (let [str-points (str/split str #" -> ")
        segments (->> str-points
                      (map #(str/split % #","))
                      (map (fn [s] (map #(Integer/parseInt % 10) s)))
                      (partition 2 1))]
    (mapcat create-line segments)))

(defn starting-grid [str]
  (->> str
       str/trim-newline
       str/split-lines
       (mapcat str->points)
       (reduce (fn [grid curr]
                 (assoc-in grid [(:y curr) (:x curr) :state] "#"))
               grid)))

(defn display-row [row]
  (str/join "" (map :state row)))

(defn display-grid [grid]
  (->> (map display-row grid)
       (str/join "\n")))

(def sand-pos (atom [0 500]))
(def sand-counter (atom 0))
(def grid-state (atom (starting-grid (slurp (io/resource "day14.txt")))))

(defn next-pos []
  (let [[y x] @sand-pos
        grid @grid-state]
    (cond 
      (= "." (get-in grid [(inc y) x :state])) [(inc y) x]
      (= "." (get-in grid [(inc y) (dec x) :state])) [(inc y) (dec x)]
      (= "." (get-in grid [(inc y) (inc x) :state])) [(inc y) (inc x)]
      :else nil)))

(defn create-sand []
  (if (= [0 500] @sand-pos)
    (do
      (println "sand-counter:" (inc @sand-counter))
      (throw (Exception. "heh, this is horrible")))
    (do
      (reset! sand-pos [0 500])
      (swap! sand-counter inc))))

(defn move-or-create-sand []
  (if-let [next (next-pos)]
    (do
      (swap! grid-state assoc-in (conj @sand-pos :state) ".")
      (reset! sand-pos next)
      (swap! grid-state assoc-in (conj next :state) "#")
      @sand-pos)
    (create-sand)))

(defn add-floor []
  (doseq [i (range 1000)]
    (swap! grid-state assoc-in [168 i :state] "#")))

(comment
(get-in @grid-state [1 500 :state])
(move-or-create-sand)
(next-pos)
(move-or-create-sand)
(add-floor)
(do 
  (while (< (first @sand-pos) 170)
    (move-or-create-sand))
  (println "sand counter:" @sand-counter))

(dotimes [n 10000]
  (move-or-create-sand))
(spit "grid.txt" (display-grid @grid-state))
(do (swap! grid-state assoc-in [1 500 :state] "#") nil)
(def filled-grid (starting-grid (slurp (io/resource "day14.txt"))))
(spit "grid.txt" (display-grid filled-grid))
;; if y > 170 it'll never stop
  ,)
