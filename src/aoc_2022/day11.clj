(ns aoc-2022.day11
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))



(def example
  "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")

(defn parse-monkey [monkey-str]
  (let [[id-str
         items-str
         op-str
         div-str
         true-str
         false-str] (str/split-lines monkey-str)]
    {:id (Integer/parseInt (re-find #"\d+" id-str) 10)
     :items (-> (str/split items-str #": ")
                second
                (str/split #", ")
                (->> (mapv #(Integer/parseInt % 10))))
     :op  (str/split (re-find #"[-+/*].*" op-str) #" ")
     :div (Integer/parseInt (re-find #"\d+" div-str) 10)
     :t (Integer/parseInt (re-find #"\d+" true-str) 10)
     :f (Integer/parseInt (re-find #"\d+" false-str) 10)}))

(defn target-monkey [item {:keys [op div t f]}]
  (let [[op m] op
        modifier (bigint (if (= m "old") item (Integer/parseInt m 10)))
        v (case op
            "+" (+ item modifier)
            "*" (* item modifier)
            "-" (- item modifier)
            "/" (/ item modifier))
        stress-mod v];;(quot v 3)
    [(if (zero? (rem stress-mod div)) t f) stress-mod]))

(defn throw-items [initial-monkey initial-monkeys]
  (loop [i 0
         {items :items :as monkey} initial-monkey
         monkeys initial-monkeys]
    (println "throw-item:" items)
    (if (empty? items)
      monkeys
      (let [[item] items
            [target new-item] (target-monkey item monkey)]
        (recur
         (inc i)
         (update monkey :items (partial drop 1))
         (->> (update-in monkeys [target :items] conj new-item)))))))

(defn run-round [initial-monkeys]
  (loop [i 0
         monkeys initial-monkeys] ;; heh, monkey tail
    (println "run-round:" i)
    (if (= i (count monkeys))
      monkeys
      (recur (inc i)
             (-> (throw-items (get monkeys i) monkeys)
                 (update-in [i :n-inspected] (fnil (partial + (count (get-in monkeys [i :items]))) 0))
                 (assoc-in [i :items] []))))))

(comment
  (->> (str/split (slurp (io/resource "day11.txt")) #"\n\n")
       (mapv parse-monkey)
       (iterate run-round)
       (drop 1)
       (take 10000))
  )
