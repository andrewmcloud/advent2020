(ns advent-2020.day3
  (:require [advent-2020.util :refer [read-string-file]]))

(def tree "#")

(defn- parse-input
  [l]
  (letfn [(build-dict [split-line]
            (reduce merge (map #(assoc {} %1 %2) (range) split-line)))]
    (->> l
         (map #(clojure.string/split % #""))
         (map build-dict))))

(defn- insert-pos
  [shift-right map-list]
  (map #(assoc %1 :pos %2)
       map-list
       (range 0 (* shift-right (count map-list)) shift-right)))

(defn- trash-odds
  [l]
  (keep-indexed #(when (even? %1) %2) l))

(defn- count-trees
  [map-list]
  (reduce (fn [tree-count row]
            (let [num-cols (dec (count row))         ;; dec to account for :pos k,v
                  pos (mod (:pos row) num-cols)]
              (if (= (get row pos) tree)
                (inc tree-count)
                tree-count)))
          0
          map-list))

(defn solve-1
  []
  (->> "day3-input.txt"
       read-string-file
       parse-input
       (insert-pos 3)
       count-trees))

(defn solve-2
  []
  (let [prep (->> "day3-input.txt"
                  read-string-file
                  parse-input)
        s1-1 (count-trees (insert-pos 1 prep))
        s3-1 (count-trees (insert-pos 3 prep))
        s5-1 (count-trees (insert-pos 5 prep))
        s7-1 (count-trees (insert-pos 7 prep))
        s1-2 (count-trees (insert-pos 1 (trash-odds prep)))]
    (* s1-1 s3-1 s5-1 s7-1 s1-2)))
