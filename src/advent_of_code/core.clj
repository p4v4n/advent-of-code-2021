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
      (str/split #"\n")))

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

;;--- Day 2: Dive! ---

(defn course-as-coordinate [course]
  (let [[dirn dist] (str/split course #"\s+")
        d (read-string dist)]
    (condp = dirn
      "up" [0 (- d)]
      "down" [0 d]
      "forward" [d 0])))

(defn add-coordinates [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn aoc3 []
  (->> (file-as-seq "resources/input-2.txt")
       (map course-as-coordinate)
       (reduce add-coordinates [0 0])
       (reduce *)))

(defn add-coordinates-with-aim [[x1 y1 a] [x2 y2]]
  [(+ x1 x2) (+ y1 (* x2 a)) (if (= x2 0) (+ a y2) a)])

(defn aoc4 []
  (->> (file-as-seq "resources/input-2.txt")
       (map course-as-coordinate)
       (reduce add-coordinates-with-aim [0 0 0])
       drop-last
       (reduce *)))

;;--- Day 3: Binary Diagnostic ---

(defn bit-str-as-arr [bit-str]
  (->> (str/split bit-str #"")
       (map read-string)))

(defn bit-arr-to-decimal [bit-arr]
  (->> (map #(* %1 (Math/pow 2 %2)) (reverse bit-arr) (range))
       (reduce +)))

(defn rotate-2d-array [arr]
  (reduce #(map conj %1 %2) (repeat []) arr))

(defn aoc5 []
  (->> (file-as-seq "resources/input-3.txt")
       (map bit-str-as-arr)
       rotate-2d-array
       (map frequencies)
       (map #(sort-by val %))
       (map #(list (-> % first first) (-> % last first)))
       rotate-2d-array
       (map bit-arr-to-decimal)
       (reduce *)
       int))

(defn filter-coll-with-pred [coll pred-fn]
  (loop [i 0 bits coll]
    (if (<= (count bits) 1)
      (first bits)
      (let [val (-> bits rotate-2d-array (nth i) frequencies pred-fn)]
        (recur (+ i 1) (->> bits (filter #(= (nth % i) val))))))))

(defn aoc6 []
  (let [all-bits (->> (file-as-seq "resources/input-3.txt")
                      (map bit-str-as-arr))
        co2-filter #(if (<= (% 0) (% 1)) 0 1)
        oxygen-filter #(if (>= (% 1) (% 0)) 1 0)
        co2-scrubber (filter-coll-with-pred all-bits co2-filter)
        oxygen-generator (filter-coll-with-pred all-bits oxygen-filter)]
    (->> [co2-scrubber oxygen-generator]
         (map bit-arr-to-decimal)
         (reduce *)
         int)))

;;------------------------------------------

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
