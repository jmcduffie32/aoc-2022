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

(defn parse-instructions [inst-str] inst-str)

(defn parse-all [data-str]
  (-> data-str
      (str/split #"\n\n")
      ((fn [[argmt-str inst-str]]
         {:argmt (parse-arrangement argmt-str)
          :inst  (parse-instructions inst-str)}))))

(defn move [stacks {:keys [from to n]}]
  (let [crates (reverse (take-last n from))]
    (-> stacks
        (update from #(drop n))
        (update to #(conj % crates)))))

(comment
  (parse-arrangement "[Z] [M] [B]\n1 2 3")
  (partition 4 "[N] [C] ")
  (str/split "[N] [C] " #" ")

  (update [[1] [2] [3]] 0 #(concat % [2]))


  (-> "day5_ex.txt"
      io/resource
      slurp
      parse-all)
  ,)
