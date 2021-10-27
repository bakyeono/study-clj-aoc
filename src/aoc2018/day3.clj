;; https://adventofcode.com/2018/day/2
(ns aoc2018.day3
  (:require [clojure.set :as s]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combinatorics]
            [util.util :as util]))

(def rectangle-pattern #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")

(defn parse-rectangle [rectangle-str]
  (let [match (re-matches rectangle-pattern rectangle-str)
        id (Integer/parseInt (nth match 1))
        x (Integer/parseInt (nth match 2))
        y (Integer/parseInt (nth match 3))
        w (Integer/parseInt (nth match 4))
        h (Integer/parseInt (nth match 5))]
    {:id    id
     :x     x
     :y     y
     :w     w
     :h     h
     :x-end (+ x w)
     :y-end (+ y h)}))

(defn parse-input [input]
  (->> input
       string/split-lines
       (map parse-rectangle)))

(defn coordinates [{:keys [x y w h] :as rectangle}]
  (for [offset-x (range w)
        offset-y (range h)]
    [(+ x offset-x)
     (+ y offset-y)]))

(defn get-collapsed-part [rectangle1 rectangle2]
  (let [x     (max (:x rectangle1) (:x rectangle2))
        y     (max (:y rectangle1) (:y rectangle2))
        x-end (min (:x-end rectangle1) (:x-end rectangle2))
        y-end (min (:y-end rectangle1) (:y-end rectangle2))]
    (when (and (< x x-end) (< y y-end))
      {:x     x
       :y     y
       :w     (- x-end x)
       :h     (- y-end y)
       :x-end x-end
       :y-end y-end})))

(defn total-area [rectangles]
  (->> rectangles
       (map coordinates)
       (reduce into)
       set
       count))

(defn find-independent-rectangles [rectangles]
  (filter (fn [rectangle]
            (let [collapsed-rectangles (filter (partial get-collapsed-part rectangle)
                                               rectangles)]
              (when (= 1 (count collapsed-rectangles))
                rectangle)))
          rectangles))

(def puzzle-input (slurp "data/aoc2018/day3.data"))

;; solve part 1
(->> puzzle-input
     parse-input
     (#(combinatorics/combinations % 2))
     (keep #(get-collapsed-part (first %) (second %)))
     total-area)

;; solve part 2
(->> puzzle-input
     parse-input
     find-independent-rectangles)
