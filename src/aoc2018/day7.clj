;; https://adventofcode.com/2018/day/7
(ns aoc2018.day7
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(def rule-pattern #"Step ([A-Z]) must be finished before step ([A-Z]) can begin.")

(defn parse-rule [rule-string]
  (let [[_ [pre & _] [post & _]] (re-matches rule-pattern rule-string)]
    [pre post]))

(defn parse-rule-pairs [input]
  (->> input
       string/split-lines
       (map parse-rule)))

(defn build-directional-graph [rule-pairs]
  (let [from-nodes (set (map first rule-pairs))
        to-nodes (set (map second rule-pairs))
        nodes (set/union from-nodes to-nodes)
        independent-nodes (set/difference nodes to-nodes)
        collect-following-nodes (fn [direction leading-node]
                                  (let [rule-pairs (case direction
                                                     :forward rule-pairs
                                                     :backward (map reverse rule-pairs))]
                                    [leading-node (->> rule-pairs
                                                       (filter #(= (first %) leading-node))
                                                       (map second)
                                                       (into #{}))]))
        forward-graph (into {} (map (partial collect-following-nodes :forward) from-nodes))
        backward-graph (into {} (map (partial collect-following-nodes :backward) to-nodes))]
    {:forward forward-graph :backward backward-graph
     :from-nodes from-nodes :to-nodes to-nodes
     :nodes nodes           :independent-nodes independent-nodes}))

(defn find-full-path [number-of-workers work-seconds graph]
  (let [workers (into [] (take number-of-workers
                               (repeat {:available-at -1 :work nil})))
        end? (fn [{:keys [done]}]
               (= (:nodes graph) (set done)))
        process (fn [{:keys [turn workers done]}]
                  (let [done (->> workers
                                  (filter #(and (= turn (:available-at %))
                                                (:work %))) ; nil 제외
                                  (map :work)
                                  (into done))
                        from-nodes-done? #(->> (set/difference (second %) (set done))
                                               empty?)
                        reachable-nodes (->> (filter from-nodes-done? (:backward graph))
                                             (map first)
                                             set)
                        available-nodes (set/difference (set/union (:independent-nodes graph) reachable-nodes)
                                                        (set done)
                                                        (set (map :work workers)))
                        available-worker-indexes (filter #(<= (:available-at (workers %)) turn)
                                                         (range (count workers)))
                        worker-index-and-node-pairs (map vector
                                                         available-worker-indexes
                                                         (sort available-nodes))
                        workers (reduce (fn [workers [index node]]
                                          (assoc workers index {:available-at (+ turn (work-seconds node))
                                                                :work node}))
                                        workers
                                        worker-index-and-node-pairs)]
                    {:turn (inc turn) :workers workers :done done}))]
    (->> {:turn -1 :workers workers :done []}
         (iterate process)
         (drop-while (complement end?))
         first)))

(defn work-seconds [base work]
  (+ base (- (int work) (int \A))))

(def puzzle-input (slurp "data/aoc2018/day7.data"))

;; solve part 1
(->> puzzle-input
     parse-rule-pairs
     build-directional-graph
     (find-full-path 1 (partial work-seconds 1))
     :done
     (reduce str))

;; solve part 2
(->> puzzle-input
     parse-rule-pairs
     build-directional-graph
     (find-full-path 5 (partial work-seconds 61))
     :turn)
