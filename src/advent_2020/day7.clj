(ns advent-2020.day7
   (:require [advent-2020.util :refer [read-string-file]]
             [clojure.string :as s]))

(defn- clean-kvs
  [s]
  (-> s
      (s/replace " " "-")
      keyword))

(defn- parse-input
  [in]
  (let [color-re #"(\w+ \w+) bag"]
    (->> in
      (map #(re-seq color-re %))
      (map (fn [l] (map second l)))
      (reduce (fn [m [k & vs]]
                (merge m {(clean-kvs k) (set (map clean-kvs vs))}))
              {}))))

(defn- can-it-hold-the-shiny-gold?
  [bag-map k]
  (cond
    (contains? (k bag-map) :no-other) false
    (contains? (k bag-map) :shiny-gold) true
    :else (reduce #(or %1 (can-it-hold-the-shiny-gold? bag-map %2)) false (k bag-map))))

(defn- search-bags
  [bag-map]
  (loop [[k & ks] (keys bag-map)
         colors []]
    (cond
      (nil? k) colors
      (contains? (k bag-map) :no-other) (recur ks colors)
      (can-it-hold-the-shiny-gold? bag-map k) (recur ks (conj colors k))
      :else (recur ks colors))))

(defn solve-1
  []
  (->> "day7-input.txt"
       read-string-file
       parse-input
       search-bags
       count))