(ns aoc-2022.day9
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(defn move [dir {:keys [x y]}]
  (case dir
    "U" {:x x :y (inc y)}
    "D" {:x x :y (dec y)}
    "L" {:x (dec x) :y y}
    "R" {:x (inc x) :y y}))

(defn create-head-path [steps]
  (loop [path [{:x 0, :y 0}]
         [head & tail] steps]
    (if (nil? head)
      (into [] path)
      (recur (concat
              path
              (->> (last path)
                   (iterate (partial move (:dir head)))
                   (drop 1)
                   (take (:n head))
                   (into [])))
             tail))))

(defn too-far? [{h-x :x h-y :y} {t-x :x t-y :y}]
  (or (< 1 (Math/abs (- h-x t-x)))
      (< 1 (Math/abs (- h-y t-y)))))

(defn move-tail [{tx :x ty :y}
                 {hx2 :x hy2 :y}
                 {hx1 :x hy1 :y}]
  (let [delta-x (- hx2 hx1)
        delta-y (- hy2 hy1)]
    (if (and (not= tx hx2)
             (not= ty hy2))
      (if (= (Math/abs delta-x) (Math/abs delta-y))
        {:x (+ tx delta-x)
         :y (+ ty delta-y)}
        {:x hx1
         :y hy1})
      (if (= tx hx2)
        {:x tx :y (+ ty delta-y)}
        {:x (+ tx delta-x) :y ty}))))

(defn create-tail-path [h-path]
  (loop [i 0
         [t-pos :as t-path] '({:x 0 :y 0})]
    (if (= i (count h-path))
      (into [] (reverse t-path))
      (if (too-far? (get h-path i) t-pos)
        (recur (inc i)
               (conj t-path
                     (move-tail t-pos
                                (get h-path i)
                                (get h-path (dec i)))))
        (recur (inc i)
               t-path)))))

(defn parse-step [s]
  (let [[_ dir n] (re-matches #"(\w) (\d+)" s)]
    {:dir dir :n (Integer/parseInt n 10)}))

(defn solve [str segments]
  (->> str
       str/split-lines
       (mapv parse-step)
       create-head-path
       (iterate create-tail-path)
       (drop segments)
       first
       (into #{})
       count))

(comment
  (solve (slurp (io/resource "day9.txt")) 1)
  (solve (slurp (io/resource "day9.txt")) 9)
  ,)
