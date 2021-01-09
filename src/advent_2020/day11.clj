(ns advent-2020.day11
  (:require [advent-2020.util :refer [read-string-file]]))

(defn- north [[row col]] [(dec row) col])
(defn- north-east [[row col]] [(dec row) (inc col)])
(defn- east [[row col]] [row (inc col)])
(defn- south-east [[row col]] [(inc row) (inc col)])
(defn- south [[row col]] [(inc row) col])
(defn- south-west [[row col]] [(inc row) (dec col)])
(defn- west [[row col]] [row (dec col)])
(defn- north-west [[row col]] [(dec row) (dec col)])

(def directions [north north-east east south-east south south-west west north-west])

(defn- adjacent-occupied?
  [seat seat-layout direction]
  (= "#" (get-in seat-layout (direction seat))))

(defn- visible-occupied?
  [seat seat-layout direction]
  (let [next (get-in seat-layout (direction seat))]
    (cond
      (nil? next) false
      (= "L" next) false
      (= "#" next) true
      :else (visible-occupied? (direction seat) seat-layout direction))))

(defn- format-seat-layout [l cols] (->> l (partition cols) (map vec) vec))
(defn- any-updates? [a b] (some false? (mapcat #(map = %1 %2) a b)))
(defn- count-occupied-seats [seat-layout] (count (keep #{"#"} (mapcat identity seat-layout))))

(defn- num-occupied
  [seat seat-layout rule directions]
  (->> directions
       (map #(rule seat seat-layout %))
       (filter true?)
       count))

(defn- apply-occupied
  [directions rule seat-layout]
  (let [rows (count seat-layout)
        cols (count (first seat-layout))]
    (format-seat-layout (for [x (range 0 rows)
                              y (range 0 cols)]
                          (num-occupied [x y] seat-layout rule directions))
                        cols)))

(defn- update-seat-layout
  [seat-layout occupied occupied-neighbors]
  (let [rows (count seat-layout)
        cols (count (first seat-layout))]
    (format-seat-layout (for [row (range 0 rows)
                              col (range 0 cols)
                              :let [nearby-occupied-seats (get-in occupied-neighbors [row col])
                                    current (get-in seat-layout [row col])]]
                          (cond
                            (and (= "L" current) (zero? nearby-occupied-seats)) "#"
                            (and (= "#" current) (>= nearby-occupied-seats occupied)) "L"
                            :else (get-in seat-layout [row col])))
                       cols)))

(defn- find-final-layout
  [directions occupied rule seat-layout]
  (let [occupied-neighbors (apply-occupied directions rule seat-layout)
        updated-layout (update-seat-layout seat-layout occupied occupied-neighbors)]
    (if (any-updates? seat-layout updated-layout)
      (find-final-layout directions occupied rule updated-layout)
      seat-layout)))

(defn- parse-input
  [input]
  (->> input read-string-file
       (map #(clojure.string/split % #""))
       vec))

(defn solve-1
  []
  (->> "day11-input.txt"
       parse-input
       (find-final-layout directions 4 adjacent-occupied?)
       count-occupied-seats))

(defn solve-2
  []
  (->> "day11-input.txt"
       parse-input
       (find-final-layout directions 5 visible-occupied?)
       count-occupied-seats))