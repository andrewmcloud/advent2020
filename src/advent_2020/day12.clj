(ns advent-2020.day12
  (:require [advent-2020.util :refer [read-string-file]]))

(def cardinal {:north 0 :east 90 :south 180 :west 270})
(def degrees {0 :north 90 :east 180 :south 270 :west})

(defn- parse-input
  [input]
  (->> input
       read-string-file
       (mapcat #(re-seq #"([A-Z])(\d+)" %))
       (map #(conj [] (second %) (Integer/parseInt (last %))))))

(defn- turn-left
  [[ _ value] facing]
  (let [new (- (facing cardinal) value)]
    (if (nat-int? new)
      (get degrees new)
      (get degrees (+ 360 new)))))

(defn- turn-right
  [[_ value] facing]
  (let [new (+ (facing cardinal) value)]
    (get degrees (mod new 360))))

(defn- move1
  [[action value :as instruction] m]
  (let [facing (:facing m)]
    (condp = action
      "N" (update m :north #(+ value %))
      "S" (update m :south #(+ value %))
      "E" (update m :east #(+ value %))
      "W" (update m :west #(+ value %))
      "L" (assoc m :facing (turn-left instruction facing))
      "R" (assoc m :facing (turn-right instruction facing))
      "F" (update m facing #(+ value %)))))

(defn- update-waypoints
  [instruction turn-fn ship-map]
  (reduce (fn [m k]
            (assoc m
              (turn-fn instruction k)
              (get-in ship-map [:waypoint k])))
          {:north 0 :east 0 :south 0 :west 0}
          [:north :east :south :west]))

(defn- apply-waypoint
  [ship-map value]
  (let [north (get-in ship-map [:waypoint :north])
        south (get-in ship-map [:waypoint :south])
        east (get-in ship-map [:waypoint :east])
        west (get-in ship-map [:waypoint :west])]
    (-> (:ship ship-map)
        (update :north #(+ (* value (- north south)) %))
        (update :east #(+ (* value (- east west)) %)))))

(defn- move2
  [[action value :as instruction] m]
  (condp = action
    "N" (update-in m [:waypoint :north] #(+ value %))
    "S" (update-in m [:waypoint :south] #(+ value %))
    "E" (update-in m [:waypoint :east] #(+ value %))
    "W" (update-in m [:waypoint :west] #(+ value %))
    "L" (assoc m :waypoint (update-waypoints instruction turn-left m))
    "R" (assoc m :waypoint (update-waypoints instruction turn-right m))
    "F" (assoc m :ship (apply-waypoint m value))))

(defn- manhattan
  [ship-map]
  (let [n-s (Math/abs ^int (- (:north ship-map) (:south ship-map)))
        e-w (Math/abs ^int (- (:east ship-map) (:west ship-map)))]
    (+ n-s e-w)))

(defn solve-1
  []
  (->> "day12-input.txt"
       parse-input
       (reduce (fn [m instruction] (move1 instruction m))
               {:facing :east :north 0 :east 0 :south 0 :west 0})
       manhattan))

(defn solve-2
  []
  (->> "day12-input.txt"
       parse-input
       (reduce (fn [m instruction] (move2 instruction m))
               {:waypoint {:north 1 :east 10 :south 0 :west 0}
                :ship {:north 0 :east 0 :south 0 :west 0}})
       :ship
       manhattan))