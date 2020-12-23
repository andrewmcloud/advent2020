(ns advent-2020.day4
  (:require [clojure.string :as s]
            [advent-2020.util :refer [read-string-file]]))

(def credentials #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"})

(defn- valid-birth? [m] (and (< 1919 (Integer/parseInt (get m "byr"))) (> 2003 (Integer/parseInt (get m "byr")))))
(defn- valid-issue? [m] (and (< 2009 (Integer/parseInt (get m "iyr"))) (> 2021 (Integer/parseInt (get m "iyr")))))
(defn- valid-expiration? [m] (and (< 2019 (Integer/parseInt (get m "eyr"))) (> 2031 (Integer/parseInt (get m "eyr")))))
(defn- valid-eyecolor? [m] (true? (some (partial = (get m "ecl")) #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})))
(defn- valid-haircolor? [m] (some? (re-matches #"\#[0-9a-f]{6}" (get m "hcl"))))
(defn- valid-passport-id? [m] (and (some? (re-find #"[0-9]{9}" (get m "pid")))))
(defn- valid-height-cm? [hgt] (and (< 149 hgt) (> 194 hgt)))
(defn- valid-height-in? [hgt] (and (< 58 hgt) (> 77 hgt)))
(defn- valid-height? [m]
  (let [hgt (get m "hgt")]
    (cond
      (re-find #"[0-9]{3}cm" hgt) (if (valid-height-cm? (Integer/parseInt (subs hgt 0 3))) true false)
      (re-find #"[0-9]{2}in" hgt) (if (valid-height-in? (Integer/parseInt (subs hgt 0 2))) true false)
      :else false)))

(def validate-fns [valid-birth? valid-issue? valid-expiration? valid-eyecolor?
                   valid-haircolor? valid-passport-id? valid-height?])

(defn- parse-passport-line-to-map
  [l]
  (->> (s/split l #" ")
       (map #(s/split % #":"))
       flatten
       (apply assoc {})))

(defn- generate-passports
  [input]
  (loop [[line & lines] input
         passports []
         passport {}]
    (cond
      (nil? line)   (conj passports passport)
      (empty? line) (recur lines
                           (conj passports passport)
                           {})
      :else         (recur lines
                           passports
                           (merge passport (parse-passport-line-to-map line))))))

(defn- valid-keys?
  [passport]
  (= credentials (disj (set (keys passport)) "cid")))

(defn- valid-passport?
  [passport]
  (if (valid-keys? passport)
    (every? true? (map #(% passport) validate-fns))
    false))

(defn solve-1
  []
  (->> "day4-input.txt"
       read-string-file
       generate-passports
       (map valid-keys?)
       (filter true?)
       count))

(defn solve-2
  []
  (->> "day4-input.txt"
       read-string-file
       generate-passports
       (map valid-passport?)
       (filter true?)
       count))