(ns advent-2020.day10
  (:require [advent-2020.util :refer [read-string-file]]))

(defn- parse-input
  [input]
  (->> input
       read-string-file
       (map #(Integer/parseInt %))))

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
          (sort joltage-list)))       ;;:processed starts with '(0) to account for the outlet

(defn- x-joltage [jm] (* (:1 jm) (:3 jm)))

(defn- insert-plug&device
  [adaptor-list]
  (let [device (+ 3 (apply max adaptor-list))
        plug 0]
    (conj adaptor-list plug device)))

(defn- build-adapter-graph
  [adaptors]
  (letfn [(valid-adaptors [adaptor]
            (filter (set (range (inc adaptor) (+ 4 adaptor))) adaptors))]
    (reduce (fn [graph adaptor]
              (if adaptor
                (assoc graph adaptor (valid-adaptors adaptor))
                graph))
            {}
            adaptors)))

(def dfs
  (memoize
    (fn [k joltage-graph]
      (if (not-empty (get joltage-graph k))
        (apply + (map #(dfs % joltage-graph) (get joltage-graph k)))
        1))))

(defn solve-1
  []
  (->> "day10-test.txt"
       parse-input
       traverse-adaptors
       x-joltage))

(defn solve-2
  []
  (->> "day10-input.txt"
       parse-input
       insert-plug&device
       build-adapter-graph
       (dfs 0)))