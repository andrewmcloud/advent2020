(ns advent-2020.day13
  (:require [advent-2020.util :refer [read-string-file]]))

(defn- clean-buslist
  [bus-list]
  (->> (clojure.string/split bus-list #",")
       (map #(try
               (Integer/parseInt %)
               (catch NumberFormatException e %)))
       vec))

(defn- parse-input
  []
  (let [in (read-string-file "day13-input.txt")
        bus-map (assoc {} :timestamp (Integer/parseInt (first in)) :bus-list (last in))]
    (update bus-map :bus-list clean-buslist)))

(defn- earliest-bus
 [timestamp bus-list]
 (loop [[bus & rest] (remove #(= "x" %) bus-list)
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

(defn- build-pairs
  [{:keys [bus-list]}]
  (->> bus-list
       (map-indexed (fn [idx num]
                      (when (int? num)
                        [(mod (- num idx) num) num])))
       (remove nil?)
       (sort-by second)
       reverse)) ;;sort moduli by decreasing value per wikipedia

;;https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Search_by_sieving
(defn CRTsieve [[result product] [num modulo]]
  (loop [result result]
    (if (= num (mod result modulo))
      [result (* product modulo)]
      (recur (+ result product)))))

(defn solve-2
  []
  (->> (parse-input)
       build-pairs
       (reduce CRTsieve)
       first))