(ns advent-2020.day5
  (:require [clojure.string :as s]
            [advent-2020.util :refer [read-string-file]]))

(defn- read-string-file [f]
  (with-open [rdr (io/reader (io/resource f))]
    (reduce conj [] (line-seq rdr))))

(defn- find-seat-number
  [seat-id lletter uletter rng]
  (loop [[letter & letters] (s/split seat-id #"")
         [l-seat u-seat] [0 (dec rng)]]
    (cond
      (nil? letter) l-seat
      (= lletter letter) (recur letters [l-seat (- u-seat (inc (quot (- u-seat l-seat) 2)))])
      (= uletter letter) (recur letters [(+ l-seat (inc (quot (- u-seat l-seat) 2))) u-seat]))))

(defn- calc
  [seat]
  (let [row (find-seat-number (subs seat 0 7) "F" "B" 128)
        col (find-seat-number (subs seat 7 10) "L" "R" 8)]
    ;;Every seat also has a unique seat ID: multiply the row by 8, then add the column.
    (+ col (* row 8))))

(defn- find-missing
  [l]
  (loop [[x & xs] l]
    (if (= (inc x) (first xs))
      (recur xs)
      (inc x))))

(defn solve-1
  []
  (->> "day5-input.txt"
       read-string-file
       (map calc)
       (apply max)))

(defn solve-2
  []
  (->> "day5-input.txt"
       read-string-file
       (map calc)
       sort
       find-missing))