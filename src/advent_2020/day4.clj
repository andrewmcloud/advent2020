(ns advent-2020.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn- read-string-file [f]
  (with-open [rdr (io/reader (io/resource f))]
    (reduce conj [] (line-seq rdr))))

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

;byr (Birth Year) - four digits; at least 1920 and at most 2002.
;iyr (Issue Year) - four digits; at least 2010 and at most 2020.
;eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
;hgt (Height) - a number followed by either cm or in:
;If cm, the number must be at least 150 and at most 193.
;If in, the number must be at least 59 and at most 76.
;hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
;ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
;pid (Passport ID) - a nine-digit number, including leading zeroes.
;102 is too high
