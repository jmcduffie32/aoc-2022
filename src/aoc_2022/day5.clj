(ns aoc-2022.day5
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(defn parse-arrangement [inst-str]
  (-> inst-str
      str/split-lines
      reverse
      (->>
       (drop 1)
       (map #(str/replace % #"    " "[0] "))
       (map #(mapcat (partial drop 1)
                     (re-seq #"\[([0A-Z])\]" %)))
       (apply map list)
       (mapv (fn [a] (filter #(not= "0" %) a))))))

(defn parse-instructions [inst-str]
  (->> inst-str
       str/split-lines
       (map #(str/split % #" "))
       (map (fn [[_ n _ from _ to]]
              {:n (Integer/parseInt n 10)
               :from (Integer/parseInt from 10)
               :to (Integer/parseInt to 10)}))))

(defn parse-all [data-str]
  (-> data-str
      (str/split #"\n\n")
      ((fn [[argmt-str inst-str]]
         {:argmt (parse-arrangement argmt-str)
          :inst  (parse-instructions inst-str)}))))

(defn move-9000 [stacks {:keys [from to n]}]
  (let [from-index (dec from)
        to-index (dec to)
        crates (reverse (take-last n (nth stacks from-index)))]
    (-> stacks
        (update from-index #(drop-last n %))
        (update to-index #(concat % crates)))))

(defn move-9001 [stacks {:keys [from to n]}]
  (let [from-index (dec from)
        to-index   (dec to)
        crates     (take-last n (nth stacks from-index))]
    (-> stacks
        (update from-index #(drop-last n %))
        (update to-index #(concat % crates)))))

(defn move-crates [move-fn]
  (let [parsed (-> "day5.txt"
                   io/resource
                   slurp
                   parse-all)]
    (loop [stacks (:argmt parsed)
           moves  (:inst parsed)]
      (if-not (seq moves)
        stacks
        (recur (move-fn stacks (first moves)) (rest moves))))))

(defn solve1 []
  (str/join (map last (move-crates move-9000))))


(defn solve2 []
  (str/join (map last (move-crates move-9001))))
