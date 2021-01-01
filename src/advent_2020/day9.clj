(ns advent-2020.day9
  (:require [advent-2020.util :refer [read-string-file]]))

(defn- any-two?
  [search-space target]
  (loop [[n & ns] search-space]
    (let [num-set (set ns)]
      (cond
        (nil? ns) false
        (contains? num-set (- target n)) true
        :else (recur ns)))))

(defn- search
  [preamble l]
  (loop [i 0]
    (let [search-space (drop i (take (+ preamble i) l))
          target (nth l (+ preamble i))]
      (if (false? (any-two? search-space target))
        target
        (recur (inc i))))))

(defn solve-1
  []
  (->> "day9-input.txt"
       read-string-file
       (map #(Integer/parseInt %))
       (search 25)))