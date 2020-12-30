(ns advent-2020.day8
  (:require [advent-2020.util :refer [read-string-file]]
            [clojure.string :as s]))

(defn- parse-program
  [program]
  (map (fn [cnt line]
         (let [[instruction jmp] (s/split line #" ")]
           {cnt {:instruction instruction
                 :jmp (Integer/parseInt jmp)
                 :visited false}}))
       (range)
       program))

(defn- mark-visited
  [instructions-map instruction-num]
  (assoc-in instructions-map [instruction-num :visited] true))

(defn- run-program
  [instructions-map]
  (loop [instructions-map instructions-map
         instruction-num 0
         acc 0]
    (let [instruction-map (get instructions-map instruction-num)
          instruction (:instruction instruction-map)]
      (cond
        (true? (:visited instruction-map)) acc
        (= "nop" instruction) (recur (mark-visited instructions-map instruction-num)
                                     (inc instruction-num)
                                     acc)
        (= "acc" instruction) (recur (mark-visited instructions-map instruction-num)
                                     (inc instruction-num)
                                     (+ acc (:jmp instruction-map)))
        (= "jmp" instruction) (recur (mark-visited instructions-map instruction-num)
                                     (+ instruction-num (:jmp instruction-map))
                                     acc)))))

(defn solve-1
  []
  (->> "day8-input.txt"
       read-string-file
       parse-program
       (reduce merge)
       run-program))