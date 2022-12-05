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
       (map #(mapcat (partial drop 1)
                     (re-seq #"\[([A-Z])\]" %)))
       ;; (drop 1)
       )))

(defn parse-instructions [inst-str] inst-str)

(defn parse-all [data-str]
  (-> data-str
      (str/split #"\n\n")
      ((fn [[argmt-str inst-str]]
         {:argmt (parse-arrangement argmt-str)
          :inst  (parse-instructions inst-str)}))))

(comment
  (parse-arrangement "[Z] [M] [B]\n1 2 3")
  (re-matches #"\[([A-Z])\]" "[Z] [M] [B]")
  (re-matches #"Z" "[Z] [M] [B]")

  (-> "day5_ex.txt"
      io/resource
      slurp
      parse-all)
  ,)
