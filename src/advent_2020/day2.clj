(ns advent-2020.day2
  (:require [clojure.java.io :as io]))

(defn- read-string-file [f]
  (with-open [rdr (io/reader (io/resource f))]
    (reduce conj [] (line-seq rdr))))

(def pwd-regex #"(?<min>[0-9]+)-(?<max>[0-9]+) (?<letter>[a-z]): (?<password>[a-z]+)")

(defn parse-input
  [l]
  (let [partial-match (partial re-matcher pwd-regex)]
    (map (fn [x]
           (let [matcher (partial-match x)]
             (if (.matches matcher)
               {:min      (.group matcher "min")
                :max      (.group matcher "max")
                :letter   (.group matcher "letter")
                :password (.group matcher "password")}
               (throw (Exception. "Can't extract password info.")))))
         l)))

(defn- build-freq-map
  [s]
  (into {}
        (for [[k v] (frequencies s)]
          [(keyword (str k)) v])))

(defn- validated-pwds
  [pwd-maps]
  (reduce (fn [valid pwd-map]
            (let [letter (keyword (:letter pwd-map))
                  mn (Integer/parseInt (:min pwd-map))
                  mx (Integer/parseInt (:max pwd-map))
                  fqs (build-freq-map (:password pwd-map))
                  lt-count (get fqs letter -1)]
              (if (and (<= mn lt-count) (>= mx lt-count))
                (conj valid (:password pwd-map))
                valid)))
          []
          pwd-maps))

(defn new-rule
  [pwd-maps]
  (reduce (fn [valid pwd-map]
            (let [letter (:letter pwd-map)
                  pwd (:password pwd-map)
                  letter-at-pos1 (str (nth pwd (dec (Integer/parseInt (:min pwd-map)))))
                  letter-at-pos2 (str (nth pwd (dec (Integer/parseInt (:max pwd-map)))))]
              (if (and (not= letter-at-pos1 letter-at-pos2)
                       (or (= letter-at-pos1 letter)
                           (= letter-at-pos2 letter)))
                (conj valid pwd)
                valid)))
          []
          pwd-maps))

(defn solve-1
  []
  (->> "day2-input.txt"
       read-string-file
       parse-input
       validated-pwds
       count))

(defn solve-2
  []
  (->> "day2-input.txt"
       read-string-file
       parse-input
       new-rule
       count))