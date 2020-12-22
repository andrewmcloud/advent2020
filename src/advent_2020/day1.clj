(ns advent-2020.day1
  (:require [advent-2020.util :refer [read-string-file]]))

(defn- string-list->int-list
  [l]
  (map #(Integer/parseInt %) l))

(defn- which2=2020
  [l]
  (loop [[x & xs] l
         end (dec (count l))]
    (let [c (nth l end)
          s (+ x c)]
      (cond
        (< s 2020) (recur x (dec end))
        (> s 2020) (recur xs (dec (count l)))
        :else (println "result: " x","c"-" (* x c))))))

(defn which3=2020
  [l]
  (loop [head 0
         start 1
         end (dec (count l))]
    (let [c1 (nth l head)
          c2 (nth l start)
          c3 (nth l end)
          s (+ c1 c2 c3)]
      (cond (< s 2020) (recur head start (dec end))
            (> s 2020) (recur (inc head) (inc start) (dec (count l)))
            :else (println "result: " c1","c2","c3"-" (* c1 c2 c3))))))

(defn- solve-1
  []
  (->> "day1-input.txt"
        read-string-file
        string-list->int-list
        sort
        reverse
        which2=2020))

(defn- solve-2
  []
  (->> "day1-input.txt"
       read-string-file
       string-list->int-list
       sort
       reverse
       which3=2020))