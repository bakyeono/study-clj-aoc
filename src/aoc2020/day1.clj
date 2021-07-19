;; https://adventofcode.com/2020/day/1
(ns aoc2020.day1
  (:require [clojure.string :as string])
  (:require [clojure.math.combinatorics :as combinatorics]))

(defn parse-input [input]
  (->> input
       string/split-lines
       (map #(Integer/parseInt %))))

(defn find-factors [target number-of-factors numbers]
  (first (filter #(= (reduce + %) target)
                 (combinatorics/combinations numbers number-of-factors))))

(defn calc-multiple-of-factors [target number-of-factors numbers]
  (reduce * (find-factors target number-of-factors numbers)))

(def puzzle-input (slurp "data/aoc2020/day1.data"))

;; solve part 1
(->> puzzle-input
     parse-input
     (calc-multiple-of-factors 2020 2))

;; solve part 2
(->> puzzle-input
     parse-input
     (calc-multiple-of-factors 2020 3))
