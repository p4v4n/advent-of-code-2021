(ns advent-of-code.core
  (:require [clojure.string :as str])
  (:gen-class))

;;--- Day 1: Sonar Sweep ---

(defn count-seq-increase [seq]
  (->> (map #(> %1 %2) (drop 1 seq) (drop-last 1 seq))
       (filter identity)
       count))

(defn file-as-seq [file-path]
  (-> (slurp file-path)
      (str/split  #"\n")))

(defn aoc1 []
  (->> (file-as-seq "resources/input-1.txt")
       (map read-string)
       count-seq-increase))

(defn count-seq-increase2 [seq]
  (->> (map + seq (drop 1 seq) (drop 2 seq))
       count-seq-increase))

(defn aoc2 []
  (->> (file-as-seq "resources/input-1.txt")
       (map read-string)
       count-seq-increase2))

;;(aoc1)
;;(aoc2)
