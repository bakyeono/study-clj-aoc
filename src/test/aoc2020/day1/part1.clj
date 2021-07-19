(ns test.aoc2020.day1.part1
  (:require [clojure.test :refer [deftest is]]
            [aoc2020.day1.day1 :refer [find-factors solve]]))

(deftest test-find-factors
  (is (= (find-factors 2020 2 [2010 8 2 10]) [2010 10]))
  (is (= (find-factors 2020 2 [1721 979 366 299 675 1456]) [1721 299])))

(deftest test-solve
  (is (= (solve [2010 8 2 10]) 20100))
  (is (= (solve [1721 979 366 299 675 1456]) 514579)))
