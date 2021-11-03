;; https://adventofcode.com/2018/day/1
(ns aoc2018.day1
  (:require [clojure.string :as string]
            [util.util :as util]))

(def puzzle-input (slurp "data/aoc2018/day1.data"))

;; solve part 1
(->> puzzle-input
     string/split-lines
     (map #(Integer/parseInt %))
     (reduce +))

(defn get-duplicated-or-store [seen-set value]
  (if (seen-set value)
    (reduced value)
    (conj seen-set value)))

;; solve part 2
(->> puzzle-input
     string/split-lines
     (map #(Integer/parseInt %))
     cycle
     (reductions +)
     (reduce get-duplicated-or-store #{}))
