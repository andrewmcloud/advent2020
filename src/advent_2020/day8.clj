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

(defn- swap-instruction
  [instructions-map swapped-index]
  (loop [i swapped-index]
    (let [instruction (get instructions-map i)]
      (cond
        (= "nop" (:instruction instruction)) (assoc-in instructions-map [i :instruction] "jmp")
        (= "jmp" (:instruction instruction)) (assoc-in instructions-map [i :instruction] "nop")
        :else (recur (inc i))))))

(defn- run-program-1
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

(defn- run-program-2
  [instructions-map]
  (loop [instrs-map instructions-map
         instruction-num 0
         acc 0
         swapped-index 0]
    (let [instruction-map (get instrs-map instruction-num)
          instruction (:instruction instruction-map)]
      (cond
        (nil? instruction-map) acc
        (true? (:visited instruction-map)) (recur (swap-instruction instructions-map swapped-index)
                                                  0
                                                  0
                                                  (inc swapped-index))
        (= "nop" instruction) (recur (mark-visited instrs-map instruction-num)
                                     (inc instruction-num)
                                     acc
                                     swapped-index)
        (= "acc" instruction) (recur (mark-visited instrs-map instruction-num)
                                     (inc instruction-num)
                                     (+ acc (:jmp instruction-map))
                                     swapped-index)
        (= "jmp" instruction) (recur (mark-visited instrs-map instruction-num)
                                     (+ instruction-num (:jmp instruction-map))
                                     acc
                                     swapped-index)))))

(defn solve-1
  []
  (->> "day8-input.txt"
       read-string-file
       parse-program
       (reduce merge)
       run-program-1))

(defn solve-2
  []
  (->> "day8-input.txt"
     read-string-file
     parse-program
     (reduce merge)
     run-program-2))