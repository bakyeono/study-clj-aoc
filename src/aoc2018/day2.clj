;; https://adventofcode.com/2018/day/2
(ns aoc2018.day2
  (:require [clojure.string :as string]
            [clojure.math.combinatorics :as combinatorics]))

(defn parse-input [input]
  (->> input
       string/split-lines
       (map #(string/split % #""))))

(defn duplicated-n-times? [n coll]
  (->> (frequencies coll)
       (filter #(= (second %) n))
       first))
(def twice? (partial duplicated-n-times? 2))
(def triple? (partial duplicated-n-times? 3))

(defn same [[a b]]
  (map #(when (= %1 %2)
          %1)
       a b))

(def puzzle-input (slurp "data/aoc2018/day2.data"))

;; solve part 1
(->> puzzle-input
     parse-input
     (#(* (count (filter twice? %))
          (count (filter triple? %)))))

;; solve part 2
(->> puzzle-input
     parse-input
     (#(combinatorics/combinations % 2))
     (map same)
     (filter #(= 1
                 (count (filter nil? %))))
     first
     (apply str))
