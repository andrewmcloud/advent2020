(ns advent-2020.day12
  (:require [advent-2020.util :refer [read-string-file]]
            [clojure.string :as s]))

(def cardinal {:north 0 :east 90 :south 180 :west 270})
(def degrees {0 :north 90 :east 180 :south 270 :west})

(defn- parse-input
  [input]
  (->> input
       read-string-file
       (mapcat #(re-seq #"([A-Z])(\d+)" %))
       (map #(conj [] (second %) (Integer/parseInt (last %))))))

(defn- turn-left
  [[ _ value] m]
  (let [facing ((:facing m) cardinal)
        new (- facing value)]
    (if (nat-int? new)
      (get degrees new)
      (get degrees (+ 360 new)))))

(defn- turn-right
  [[_ value] m]
  (let [facing ((:facing m) cardinal)
        new (+ facing value)]
    (get degrees (mod new 360))))

(defn- move
  [[action value :as instruction] m]
  (condp = action
    "N" (update m :north #(+ value %))
    "S" (update m :south #(+ value %))
    "E" (update m :east #(+ value %))
    "W" (update m :west #(+ value %))
    "L" (assoc m :facing (turn-left instruction m))
    "R" (assoc m :facing (turn-right instruction m))
    "F" (update m (:facing m) #(+ value %))))

(defn- manhattan
  [m]
  (let [n-s (Math/abs ^int (- (:north m) (:south m)))
        e-w (Math/abs ^int (- (:east m) (:west m)))]
    (+ n-s e-w)))

(defn solve
  []
  (->> "day12-input.txt"
       parse-input
       (reduce (fn [m instruction]
                 (move instruction m))
               {:facing :east :north 0 :east 0 :south 0 :west 0})
       manhattan))