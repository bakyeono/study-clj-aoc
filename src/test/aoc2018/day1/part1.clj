(ns test.aoc2018.day1.part1
  (:require [clojure.test :refer [deftest is]]
            [aoc2018.day1.part1 :refer [calc-frequency]]))

(deftest test-calc-frequencies
  (is (= (calc-frequency [+1 +1 +1]) 3))
  (is (= (calc-frequency [+1 +1 -2]) 0))
  (is (= (calc-frequency [-1 -2 -3]) -6)))
