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

(defn- subsequence-sum
  [target l]
  (loop [[n & ns] l
          i 1
          sum 0
          subsequence []]
    (cond
      (= target sum) subsequence
      (> target sum) (recur ns i (+ sum n) (conj subsequence n))
      (< target sum) (recur (drop i l) (inc i) 0 []))))

(defn- encryption-weakness
  [subsequence]
  (let [smallest (apply min subsequence)
        largest (apply max subsequence)]
    (+ smallest largest)))

(defn solve-1
  []
  (->> "day9-input.txt"
       read-string-file
       (map #(Long/parseLong %))
       (search 25)))

(defn solve-2
  []
  (->> "day9-input.txt"
       read-string-file
       (map #(Long/parseLong %))
       (subsequence-sum (solve-1))
       encryption-weakness))