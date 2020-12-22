(ns advent-2020.util
  (:require [clojure.java.io :as io]))

(defn read-string-file [f]
  (with-open [rdr (io/reader (io/resource f))]
    (reduce conj [] (line-seq rdr))))