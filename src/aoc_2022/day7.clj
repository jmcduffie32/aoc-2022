(ns aoc-2022.day7
  (:require
   [clojure.string :as str]))

(defn parse-command [line-str]
  (let [[_ cmd] (re-find #"\$ (\w+)" line-str)
        [_ arg] (re-find #"\$ \w+\s(.*)" line-str)]
    {:type :command :cmd cmd :arg arg}))

(defn parse-dir [line-str]
  {:type :dir :name (nth (re-matches #"dir (\w+)" line-str) 1)})

(defn parse-file [line-str]
  (let [[_ size-str name] (re-matches #"(\d+) (.+)" line-str)]
    {:type :file
     :name name
     :size (Integer/parseInt size-str 10)}))

(defn parse-line [line-str]
  (case (first line-str)
    \$ (parse-command line-str)
    \d (parse-dir line-str)
    (parse-file line-str)))

(defn apply-inst [{:keys [type cmd arg size]} state]
  (case [type cmd]
    [:command "cd"] (case arg
                      ".." (update state :pwd (partial drop 1))
                      "/"  (assoc state :pwd '("/"))
                      (update state :pwd conj arg))
    [:file nil]     (assoc state :sizes
                           (loop [pwd   (:pwd state)
                                  sizes (:sizes state)]
                             (if (nil? (first pwd))
                               sizes
                               (recur (rest pwd)
                                      (update sizes
                                              (str/join  "-" pwd)
                                              (fnil + 0) size)))))
    state))

(defn process [parsed]
  (loop [[head & tail] parsed
         state         {:pwd   '()
                        :sizes {}}]
    (if (nil? head)
      state
      (recur tail (apply-inst head state)))))

(defn find-sizes [str]
  (->> str
       str/split-lines
       (map parse-line)
       process
       :sizes))

(defn solve1 [str]
  (->> str
       find-sizes
       (filter (fn [[_ size]] (<= size 100000)))
       (map second)
       (apply +)))

(defn solve2 [str]
  (let [sizes  (find-sizes str)
        total  70000000
        used   (get sizes "/")
        avail  (- total used)
        needed (- 30000000 avail)]
    (->>
      sizes
      (filter (fn [[_ size]] (>= size needed)))
      (apply min-key second)
      second)))
