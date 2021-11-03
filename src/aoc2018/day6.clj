;; https://adventofcode.com/2018/day/6
(ns aoc2018.day6
  (:require [clojure.string :as string]))

(defn parse-coordinate [coordinate-str]
  (->> (string/split coordinate-str #",\s*")
       (mapv #(Integer/parseInt %))))

(defn parse-coordinates [puzzle-input]
  (->> puzzle-input
       string/split-lines
       (map parse-coordinate)))

(defn boundary [coordinates]
  {:x-start (apply min (map first coordinates))
   :x-end (inc (apply max (map first coordinates)))
   :y-start (apply min (map second coordinates))
   :y-end (inc (apply max (map second coordinates)))})

(defn init-plane [coordinates]
  (let [coordinates (vec coordinates)
        {:keys [x-start x-end y-start y-end]} (boundary coordinates)]
    (into {} (for [x (range x-start x-end)
                   y (range y-start y-end)
                   :let [coordinate [x y]
                         index (.indexOf coordinates coordinate)
                         value (if (= index -1)
                                 :unfilled
                                 index)]]
               [coordinate value]))))

(defn vicinity [[x y]]
  (->> [[x (dec y)] [x (inc y)] [(dec x) y] [(inc x) y]]))

(defn pull-vicinity-value [coordinate plane]
  (let [vicinity-values (->> (vicinity coordinate)
                             (keep plane)
                             (filter number?)
                             set)]
    (case (count vicinity-values)
      0 :unfilled
      1 (first vicinity-values)
      2 :balanced
      3 :balanced
      4 :balanced)))

(defn expand [plane]
  (->> plane
       (map (fn [[coordinate value]]
              (if (= value :unfilled)
                [coordinate (pull-vicinity-value coordinate plane)]
                [coordinate value])))
       (into {})))

(defn fixed-point [f x]
  (reduce #(if (= %1 %2)
             (reduced %1)
             %2)
          (iterate f x)))

(defn print-plane [plane]
  (let [lines-by-y (group-by (comp second key) plane)]
    (doseq [[y line] (sort lines-by-y)]
      (doseq [[x value] (sort line)]
        (print (case value
                 :unfilled ".  "
                 :balanced "++ "
                 (format "%02d " value))))
      (println))))

(comment
  (->> [[0 0] [5 5] [8 8] [6 14] [15 15]]
       init-plane
       #_expand
       #_expand
       #_expand
       #_(fixed-point expand)
       print-plane))

(def puzzle-input (slurp "data/aoc2018/day6.data"))

(defn calc-taken-area [plane]
  (->> plane
       (filter #(number? (val %)))
       (group-by val)
       (map #(vector (key %) (count (val %))))
       (into {})))

;; solve part 1
(comment
  (->> puzzle-input
       parse-coordinates
       init-plane
       (fixed-point expand)
       calc-taken-area
       vals
       (apply max)))


(defn distance [[ax ay] [bx by]]
  (+ (Math/abs (- ax bx))
     (Math/abs (- ay by))))

(defn total-distance [coordinates coordinate]
  (->> coordinates
       (map (partial distance coordinate))
       (reduce +)))

;; solve part 2
(comment
  (let [coordinates (->> puzzle-input
                         parse-coordinates)
        plane-coordinates (keys (init-plane coordinates))]
    (->> plane-coordinates
         (map (partial total-distance coordinates))
         (filter #(< % 10000))
         count)))
