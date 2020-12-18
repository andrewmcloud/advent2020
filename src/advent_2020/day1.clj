(ns advent-2020.day1
  (:require [clojure.java.io :as io]))

(defn- read-string-file [f]
  (with-open [rdr (io/reader (io/resource f))]
    (reduce conj [] (line-seq rdr))))


(defn- string-list->int-list
  [l]
  (map #(Integer/parseInt %) l))

(defn- build-sorted-list
  []
  (->> "day1-input.txt"
       read-string-file
       string-list->int-list
       sort
       reverse))

(defn solve-1
  [l]
  (loop [start 0
         end (dec (count l))]
    (let [c1 (nth l start)
          c2 (nth l end)
          s (+ c1 c2)]
      (cond
        (< s 2020) (recur start (dec end))
        (> s 2020) (recur (inc start) (dec (count l)))
        :else (println "result: " c1","c2"-" (* c1 c2))))))

(defn solve-2
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


