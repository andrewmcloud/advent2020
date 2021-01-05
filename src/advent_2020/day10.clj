(ns advent-2020.day10
  (:require [advent-2020.util :refer [read-string-file]]))

(defn- parse-input
  [input]
  (->> input
       read-string-file
       (map #(Integer/parseInt %))
       sort))

(defn- update-joltage-map
  [jm n joltage-key]
  (-> jm
      (update joltage-key inc)
      (update :processed #(cons n %))))

(defn- traverse-adaptors
  [joltage-list]
  (reduce (fn [d n]
            (cond
              (= 1 (- n (first (:processed d)))) (update-joltage-map d n :1)
              (= 3 (- n (first (:processed d)))) (update-joltage-map d n :3)
              :else d))
          {:processed '(0) :1 0 :3 1} ;;:3 starts at 1 to account for device joltage;
          joltage-list))              ;;:processed starts with '(0) to account for the outlet

(defn- x-joltage [jm] (* (:1 jm) (:3 jm)))

(defn solve-1
  []
  (->> "day10-input.txt"
       parse-input
       traverse-adaptors
       x-joltage))