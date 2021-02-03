(ns advent-2020.day14
  (:require [advent-2020.util :refer [read-string-file]]
            [clojure.string :as s]))

(defn- clean-mask
  [mask]
  (-> mask
      (s/split #" = ")
      second
      vec))

(defn- clean-mem
  [mem]
  (->> mem
       (re-seq #"\d+")
       (map #(Long/parseLong %))
       vec))

(defn- parse-input
  [in]
  (loop [[line & lines] in
         ml []
         mask-map nil]
    (if line
      (condp = (apply str (take 2 line))
        "ma" (recur lines (conj ml mask-map) {:mask (clean-mask line)})
        "me" (recur lines ml (update mask-map :mem #(conj % (clean-mem line))))
        (recur lines ml mask-map))
      (remove nil? (conj ml mask-map)))))

(defn- apply-mask
  [mem-map {:keys [mem mask]}]
  (let [and-mask (Long/parseLong (s/join (replace {\X \1 \1 \0} mask)) 2)
        or-mask (Long/parseLong (s/join (replace {\X \0} mask)) 2)]
    (merge mem-map
           (reduce (fn [m [addr val]]
                     (assoc m addr (bit-or or-mask
                                           (bit-and and-mask val))))
                   {}
                   mem))))

(defn solve
  []
  (->> "day14-input.txt"
       read-string-file
       parse-input
       (reduce apply-mask {})
       vals
       (reduce +)))