;; https://adventofcode.com/2018/day/5
(ns aoc2018.day5
  (:require [clojure.string :as string]))

(def unit-types "abcdefghijklmnopqrstuvwxyz")

(def reactive-patterns
  (->> unit-types
       (map str)
       (map #(vector (re-pattern (str % (.toUpperCase %)))
                     (re-pattern (str (.toUpperCase %) %))))
       (reduce into)))

(defn react-to-a-pattern [polymer reactive-pattern]
  (string/replace polymer reactive-pattern ""))

(defn react [polymer]
  (reduce react-to-a-pattern polymer reactive-patterns))

(defn iterate-to-last [f x]
  (reduce #(if (= %1 %2)
             (reduced %1)
             %2)
          (iterate f x)))

(defn shrink [polymer]
  (iterate-to-last react polymer))


(def puzzle-input (slurp "data/aoc2018/day5.data"))

;; solve part 1
(->> puzzle-input
     shrink
     count)

(defn remove-a-unit-type [unit-type polymer]
  (-> polymer
      (string/replace (str unit-type) "")
      (string/replace (.toUpperCase (str unit-type)) "")))

(defn find-problematic-unit-type [polymer]
  (->> unit-types
       (map #(hash-map :removed-unit-type %
                       :polymer-length (->> polymer
                                            (remove-a-unit-type %)
                                            shrink
                                            count)))
       (apply min-key :polymer-length)))

;; solve part 2
(->> puzzle-input
     find-problematic-unit-type)
