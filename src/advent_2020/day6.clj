(ns advent-2020.day6
  (:require [clojure.string :as s]
            [advent-2020.util :refer [read-string-file]]
            [clojure.set :as st]))

(defn- intersect [l] (apply st/intersection (map set l)))
(defn- union [l] (apply st/union (map set l)))

(defn- customs-declarations
  [trans-fn input]
  (loop [[line & lines] input
         declarations []
         declaration []]
    (cond
      (nil? line)   (conj declarations (trans-fn declaration))
      (empty? line) (recur lines
                           (conj declarations (trans-fn declaration))
                           [])
      :else         (recur lines
                           declarations
                           (conj declaration (s/split line #""))))))

(defn solve-1
  []
  (->> "day6-input.txt"
       read-string-file
       (customs-declarations union)
       (map count)
       (apply +)))

(defn solve-2
  []
  (->> "day6-input.txt"
       read-string-file
       (customs-declarations intersect)
       (map count)
       (apply +)))