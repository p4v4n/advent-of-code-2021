(ns advent-of-code.core
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
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

;;--- Day 4: Giant Squid ---

(defn board-str-to-vec [bingo-str]
  (->> (str/split bingo-str #"\n")
       (map #(->> (str/split (str/trim %) #"\s+")
                  (map read-string)))))

(defn numbers-till-bingo [board numbers]
  (let [win-sets (->> (concat board (rotate-2d-array board))
                      (map set))]
    (loop [i 5]
      (cond
        (> i (count numbers)) nil
        (some #(set/subset? % (set (take i numbers))) win-sets) i
        :else (recur (+ i 1))))))

(defn bingo-score [board numbers]
  (let [marked (set/intersection (set numbers) (set (apply concat board)))
        total-sum (->> board
                       (apply concat)
                       (reduce +))]
    (* (last numbers) (- total-sum (reduce + marked)))))

(defn aoc7 []
  (let [input (-> (slurp "resources/input-4.txt")
                  (str/split #"\n\n"))
        numbers (->> (str/split (nth input 0) #",")
                     (map read-string))
        boards (->> (drop 1 input)
                    (map board-str-to-vec))
        numbers-till-win (mapv #(numbers-till-bingo % numbers) boards)
        [winning-index numbers-count] (apply min-key second (map-indexed vector numbers-till-win))]
    (bingo-score (nth boards winning-index) (take numbers-count numbers))))

(defn aoc8 []
  (let [input (-> (slurp "resources/input-4.txt")
                  (str/split #"\n\n"))
        numbers (->> (str/split (nth input 0) #",")
                     (map read-string))
        boards (->> (drop 1 input)
                    (map board-str-to-vec))
        numbers-till-win (mapv #(numbers-till-bingo % numbers) boards)
        [winning-index numbers-count] (apply max-key second (map-indexed vector numbers-till-win))]
    (bingo-score (nth boards winning-index) (take numbers-count numbers))))

;;--- Day 5: Hydrothermal Venture ---

(defn input-str-to-points [input]
  (->> (str/split input #"->")
       (map #(str/split (str/trim %) #","))
       (map #(map read-string %))))

(defn vent-line [[x1 y1] [x2 y2]]
  (cond
    (= y1 y2) (->> (range (min x1 x2) (+ 1 (max x1 x2)))
                   (map #(vector % y1)))
    (= x1 x2) (->> (range (min y1 y2) (+ 1 (max y1 y2)))
                   (map #(vector x1 %)))
    :else '()))

(defn mark-vent [board vent-ends vent-fn]
  (loop [b board points (apply vent-fn vent-ends)]
    (if (empty? points)
      b
      (recur (update-in b (first points) inc) (rest points)))))

(defn aoc9 []
  (let [all-vents (->> (file-as-seq "resources/input-5.txt")
                       (map input-str-to-points))
        initial-board (vec (repeat 1000 (vec (repeat 1000 0))))
        final-board (loop [board initial-board vents all-vents]
                      (if (empty? vents)
                        board
                        (recur (mark-vent board (first vents) vent-line) (rest vents))))]
    (->> final-board
         (reduce concat)
         (filter #(> % 1))
         count)))

(defn vent-line-with-diag [[x1 y1] [x2 y2]]
  (cond
    (= y1 y2) (->> (range (min x1 x2) (+ 1 (max x1 x2)))
                   (map #(vector % y1)))
    (= x1 x2) (->> (range (min y1 y2) (+ 1 (max y1 y2)))
                   (map #(vector x1 %)))
    (= (Math/abs (- x1 x2)) (Math/abs (- y1 y2))) (let [x-sign (if (< x1 x2) 1 -1)
                                                        y-sign (if (< y1 y2) 1 -1)]
                                                    (map #(vector %1 %2)
                                                         (range x1 (+ x2 x-sign) x-sign)
                                                         (range y1 (+ y2 y-sign) y-sign)))
    :else '()))

(defn aoc10 []
  (let [all-vents (->> (file-as-seq "resources/input-5.txt")
                       (map input-str-to-points))
        initial-board (vec (repeat 1000 (vec (repeat 1000 0))))
        final-board (loop [board initial-board vents all-vents]
                      (if (empty? vents)
                        board
                        (recur (mark-vent board (first vents) vent-line-with-diag) (rest vents))))]
    (->> final-board
         (reduce concat)
         (filter #(> % 1))
         count)))

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
