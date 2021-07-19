;; https://adventofcode.com/2018/day/1
(ns aoc2018.day1
  (:require [clojure.string :as string]
            [util.util :as util]))

(def puzzle-input (slurp "data/aoc2018/day1.data"))

(defn parse-input [input]
  (->> input
       string/split-lines
       (map #(Integer/parseInt %))))

(defn reduce-frequency-changes [changes]
  (reduce + changes))

(defn history [start changes]
  (reductions + start changes))

(defn solve-part1 [puzzle-input]
  (->> puzzle-input
       parse-input
       reduce-frequency-changes))

(defn solve-part2 [puzzle-input]
  (->> puzzle-input
       parse-input
       cycle
       (history 0)
       util/get-first-duplicated))

(solve-part1 puzzle-input)
(solve-part2 puzzle-input)
