(ns advent-2020.day13
  (:require [advent-2020.util :refer [read-string-file]]))

(defn- clean-buslist
  [bus-list]
  (->> (clojure.string/split bus-list #",")
       (remove #(= "x" %))
       (map #(Integer/parseInt %))))

(defn- parse-input
  []
  (let [in (read-string-file "day13-input.txt")
        bus-map (assoc {} :timestamp (Integer/parseInt (first in)) :bus-list (last in))]
    (update bus-map :bus-list clean-buslist)))

(defn earliest-bus
 [timestamp bus-list]
 (loop [[bus & rest] bus-list
        time (Integer/MAX_VALUE)
        earliest -1]
     (cond
       (nil? bus) (* earliest time)
       (< (- bus (rem timestamp bus)) time) (recur rest (- bus (rem timestamp bus)) bus)
       :else (recur rest time earliest))))

(defn solve-1
  []
  (let [bus-schedule (parse-input)]
    (earliest-bus (:timestamp bus-schedule) (:bus-list bus-schedule))))