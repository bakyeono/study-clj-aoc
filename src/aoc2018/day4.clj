;; https://adventofcode.com/2018/day/4
(ns aoc2018.day4
  (:require [clojure.string :as string]))

(def log-entry-pattern
  #"\[(\d{4}-\d{2}-\d{2} \d{2}:\d{2})\] (.*)")

(defn coerce-minute [timestamp-string]
  (->> timestamp-string
       (re-matches #"\d{4}-\d{2}-\d{2} \d{2}:(\d{2})")
       second
       Integer/parseInt))

(defn coerce-action
  [action-string]
  (if (re-matches #"Guard #\d+ begins shift" action-string)
    :switch
    (cond (= action-string "falls asleep") :sleep-start
          (= action-string "wakes up") :sleep-end)))

(defn coerce-guard [action-string]
  (when-let [matches (re-matches #"Guard #(\d+) begins shift" action-string)]
    (Integer/parseInt (second matches))))

(defn parse-logs [logs]
  (->> logs
       string/split-lines
       sort
       (map #(re-matches log-entry-pattern %))
       (map #(hash-map :minute (coerce-minute (nth % 1))
                       :action (coerce-action (nth % 2))
                       :guard  (coerce-guard (nth % 2))))))

(defn collect-patterns [history]
  (reduce (fn [{patterns :patterns
                {prev-action :action prev-guard :guard prev-minute :minute} :prev-event}
               {:keys [action guard minute] :as new-event}]
            {:patterns (if (and (= prev-action :sleep-start)
                                (#{:switch :sleep-end} action))
                         (conj patterns {:guard prev-guard
                                         :start prev-minute
                                         :end minute
                                         :duration (- minute prev-minute)})
                         patterns)
             :prev-event (assoc new-event :guard (or guard prev-guard))})
          {:patterns []
           :prev-event {:guard nil :action nil :minute nil}}
          history))

(defn collect-patterns-by-guard [history]
  (->> (collect-patterns history)
       :patterns
       (group-by :guard)))

(defn sum-duration [patterns]
  (->> patterns
       (map :duration)
       (reduce + 0)))

(defn get-highest-frequency [patterns]
  (->> patterns
       (mapcat #(range (:start %) (:end %)))
       frequencies
       (apply max-key val)))

(defn get-best-guard-by [compare-by grouped-patterns]
  (apply max-key (comp compare-by second) grouped-patterns))

(def get-most-slept-guard (partial get-best-guard-by sum-duration))
(def get-most-frequently-slept-guard (partial get-best-guard-by (comp second get-highest-frequency)))

(defn get-most-frequently-sleeping-minute-of-guard [[guard pattern]]
  [guard (first (get-highest-frequency pattern))])

(def puzzle-input (slurp "data/aoc2018/day4.data"))

;; solve part 1
(->> puzzle-input
     parse-logs
     collect-patterns-by-guard
     get-most-slept-guard
     get-most-frequently-sleeping-minute-of-guard
     (reduce *))

;; solve part 2
(->> puzzle-input
     parse-logs
     collect-patterns-by-guard
     get-most-frequently-slept-guard
     get-most-frequently-sleeping-minute-of-guard
     (reduce *))
