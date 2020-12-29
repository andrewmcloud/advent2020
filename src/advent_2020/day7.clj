(ns advent-2020.day7
   (:require [advent-2020.util :refer [read-string-file]]
             [clojure.string :as s]))

(defn- keywordize-kvs
  [s]
  (-> s
      (s/replace " " "-")
      keyword))

(defn- build-bag-map
  [inner]
  (if (= "no other bags." (first inner))
    {}
    (reduce (fn [m inner-bags]
              (let [[_ cnt bag] (re-find #"(\d+) (\w+ \w+)" inner-bags)
                    cnt (Integer/parseInt cnt)
                    bag (keywordize-kvs bag)]
                (assoc m bag cnt)))
            {}
            inner)))

(defn- parse-input
  [in]
  (->> in
       (map #(s/split % #" bags contain "))
       (reduce (fn [m [outer inner]]
                 (let [k (keywordize-kvs outer)
                       bags (s/split inner #", ")]
                   (assoc m k (build-bag-map bags))))
               {})))

(defn- can-it-hold-the-shiny-gold?
  [bag-map k]
  (cond
    (empty? (get bag-map k)) false
    (:shiny-gold (get bag-map k)) true
    :else (reduce #(or %1 (can-it-hold-the-shiny-gold? bag-map %2)) ;;if any are true in call stack
                  false
                  (keys (get bag-map k)))))

(defn- search-bags
  [bag-map]
  (loop [[k & ks] (keys bag-map)
         colors []]
    (cond
      (nil? k) colors
      (empty? (get bag-map k)) (recur ks colors)
      (can-it-hold-the-shiny-gold? bag-map k) (recur ks (conj colors k))
      :else (recur ks colors))))

(defn- count-bags
  [outer bag-map]
  (let [inner-bag-map (get bag-map outer)]
    (if (empty? inner-bag-map)
      1
      (reduce + 1 (map (fn [inner]
                         (* (get inner-bag-map inner)
                            (count-bags inner bag-map)))
                       (keys inner-bag-map))))))

(defn solve-1
  []
  (->> "day7-input.txt"
       read-string-file
       parse-input
       search-bags
       count))

(defn solve-2
  []
  (->> "day7-input.txt"
       read-string-file
       parse-input
       (count-bags :shiny-gold)
       dec)) ;;remove the shiny-gold bag from count