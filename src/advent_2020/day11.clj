(ns advent-2020.day11
  (:require [advent-2020.util :refer [read-string-file]]))

(defn- north? [[row col] seat-map] (= "#" (get-in seat-map [(dec row) col])))
(defn- north-east? [[row col] seat-map] (= "#" (get-in seat-map [(dec row) (inc col)])))
(defn- east? [[row col] seat-map] (= "#" (get-in seat-map [row (inc col)])))
(defn- south-east? [[row col] seat-map] (= "#" (get-in seat-map [(inc row) (inc col)])))
(defn- south? [[row col] seat-map] (= "#" (get-in seat-map [(inc row) col])))
(defn- south-west? [[row col] seat-map] (= "#" (get-in seat-map [(inc row) (dec col)])))
(defn- west? [[row col] seat-map] (= "#" (get-in seat-map [row (dec col)])))
(defn- north-west? [[row col] seat-map] (= "#" (get-in seat-map [(dec row) (dec col)])))

(defn- format-seat-layout [l cols] (->> l (partition cols) (map vec) vec))
(defn- any-updates? [a b] (some false? (mapcat #(map = %1 %2) a b)))
(defn- count-occupied-seats [seat-layout] (count (keep #{"#"} (mapcat identity seat-layout))))

(defn- num-adjacent-occupied
  [seat seat-layout]
  (let [cardinal [north? north-east? east? south-east? south? south-west? west? north-west?]]
    (->> cardinal
         (map #(% seat seat-layout))
         (filter true?)
         count)))

(defn- apply-occupied
  [seat-layout]
  (let [rows (count seat-layout)
        cols (count (first seat-layout))]
    (format-seat-layout (for [x (range 0 rows)
                              y (range 0 cols)]
                          (num-adjacent-occupied [x y] seat-layout))
                        cols)))

(defn- update-seat-layout
  [seat-layout occupied-neighbors]
  (let [rows (count seat-layout)
        cols (count (first seat-layout))]
    (format-seat-layout (for [row (range 0 rows)
                              col (range 0 cols)
                              :let [nearby-occupied-seats (get-in occupied-neighbors [row col])
                                    current (get-in seat-layout [row col])]]
                          (cond
                            (and (= "L" current) (zero? nearby-occupied-seats)) "#"
                            (and (= "#" current) (> nearby-occupied-seats 3)) "L"
                            :else (get-in seat-layout [row col])))
                       cols)))

(defn- find-final-layout
  [seat-layout]
  (let [occupied-neighbors (apply-occupied seat-layout)
        updated-layout (update-seat-layout seat-layout occupied-neighbors)]
    (if (any-updates? seat-layout updated-layout)
      (find-final-layout updated-layout)
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
       find-final-layout
       count-occupied-seats))