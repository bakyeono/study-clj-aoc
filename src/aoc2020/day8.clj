;; https://adventofcode.com/2020/day/8
(ns aoc2020.day8
  (:require [clojure.string :as string])
  (:use [util util]))

(defn parse-program-line [line]
  [(keyword (first line))
   (Integer/parseInt (second line))])

(defn parse-program [source]
  (->> (string/split source #"\s")
       (partition 2)
       (map parse-program-line)))

(def get-instruction nth)

(defn init-process [program]
  {:program program :pc 0 :history [] :acc 0})

(defn step [{:keys [program pc history acc] :as process}]
  (let [[opcode operand] (get-instruction program pc)
        process (assoc process :history (conj history pc))]
    (case opcode
      :nop (assoc process :pc (inc pc)       :acc acc)
      :acc (assoc process :pc (inc pc)       :acc (+ acc operand))
      :jmp (assoc process :pc (+ pc operand) :acc acc))))

(comment
  @(def puzzle-input (slurp "data/aoc2020/day8.data"))

  ;; solve part 1
  (->> puzzle-input
       parse-program
       init-process
       (iterate step)
       (filter #(get-first-duplicated (:history %)))
       first
       :acc))
