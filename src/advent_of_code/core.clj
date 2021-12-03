(ns advent-of-code.core
  (:require [clojure.string :as str])
  (:gen-class))

;;--- Day 1: Sonar Sweep ---

(defn count-seq-increase [seq]
  (->> (map > (drop 1 seq) (drop-last 1 seq))
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

;;--------------------------------------------

(defn all-answers []
  (->> (keys (ns-publics 'advent-of-code.core))
       (filter #(str/starts-with? (str %) "aoc"))
       sort
       (map #((->> %
                   (str "advent-of-code.core/")
                   symbol
                   resolve)))))

(defn print-all-answers []
  (->> (all-answers)
       (mapv #(println %1 ">>" %2) (range 1 51))))

(defn -main [& args]
  (print-all-answers))
