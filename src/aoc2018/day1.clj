;; https://adventofcode.com/2018/day/1
(ns aoc2018.day1
  (:require [clojure.string :as string]
            [util.util :as util]))

(defn parse-input [input]
  (->> input
       string/split-lines
       (map #(Integer/parseInt %))))

(defn reduce-frequency-changes [changes]
  (reduce + changes))

(defn history [start changes]
  (reductions + start changes))

(def puzzle-input (slurp "data/aoc2018/day1.data"))

;; solve part 1
(->> puzzle-input
     parse-input
     reduce-frequency-changes)

;; solve part 2
(->> puzzle-input
     parse-input
     cycle
     (history 0)
     util/get-first-duplicated)
