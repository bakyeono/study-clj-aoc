;; https://adventofcode.com/2018/day/6
(ns aoc2018.day6
  (:require [clojure.string :as string]))

;; coordinate -- 좌표. [x y] 형태의 벡터
;;

;; plane -- 좌표평면. {coordinate: value} 형태의 맵
;;   이 때 value는 수(정점번호) 또는 키워드(특수 상태)다.
;;   특수 상태는 다음과 같다.
;;   :unfilled -- 아직 어느 정점에 속하는지 확인하지 않은 상태
;;   :balanced -- 정점 사이의 균형점에 있어, 정점에 속하지 않는 상태
;;   :boundary -- 경계 밖으로 나간 정점에 속한 상태
;;

(defn fixed-point [f x]
  (reduce #(if (= %1 %2)
             (reduced %1)
             %2)
          (iterate f x)))

(defn parse-coordinate [coordinate-str]
  (->> (string/split coordinate-str #",\s*")
       (mapv #(Integer/parseInt %))))

(defn parse-coordinates [puzzle-input]
  (->> puzzle-input
       string/split-lines
       (map parse-coordinate)))

(defn boundary [coordinates]
  {:x-start (apply min (map first coordinates))
   :x-end   (apply max (map first coordinates))
   :y-start (apply min (map second coordinates))
   :y-end   (apply max (map second coordinates))})

(defn init-plane [coordinates]
  (let [coordinates (vec coordinates)
        {:keys [x-start x-end y-start y-end]} (boundary coordinates)]
    (into {} (for [x (range x-start (inc x-end))
                   y (range y-start (inc y-end))
                   :let [coordinate [x y]
                         index (.indexOf coordinates coordinate)
                         value (if (= index -1)
                                 :unfilled
                                 index)]]
               [coordinate value]))))

(defn vicinity [[x y]]
  [[x (dec y)] [x (inc y)] [(dec x) y] [(inc x) y]])

(defn pull-vicinity-value [coordinate plane]
  (let [vicinity-values (->> (vicinity coordinate)
                             (keep plane)
                             (filter number?)
                             set)]
    (case (count vicinity-values)
      0 :unfilled
      1 (first vicinity-values)
      2 :balanced
      3 :balanced
      4 :balanced)))

(defn expand [plane]
  (->> plane
       (map (fn [[coordinate value]]
              (if (= value :unfilled)
                [coordinate (pull-vicinity-value coordinate plane)]
                [coordinate value])))
       (into {})))

(defn find-boundary-values [plane]
  (let [{:keys [x-start x-end y-start y-end]} (->> plane keys boundary)]
    (->> plane
         (keep (fn [[[x y] value]]
                 (when (or (= x x-start) (= x x-end) (= y y-start) (= y y-end))
                   value)))
         set)))

(defn mark-boundary-values [plane]
  (let [boundary-values (find-boundary-values plane)]
    (->> plane
         (map (fn [[_coordinate value]]
                   (if (get boundary-values value)
                     [_coordinate :boundary]
                     [_coordinate value])))
         (into {}))))

(defn print-plane [plane]
  (let [lines-by-y (group-by (comp second key) plane)]
    (doseq [[_y line] (sort lines-by-y)]
      (doseq [[_x value] (sort line)]
        (print (case value
                 :unfilled ".  "
                 :balanced " / "
                 :boundary "~~ "
                 (format "%02d " value))))
      (println))))

(comment
  (->> [[0 0] [5 5] [8 8] [6 14] [13 1] [15 15]]
       init-plane
       expand
       #_expand
       #_expand
       #_(fixed-point expand)
       #_mark-boundary-values
       print-plane))

(def puzzle-input (slurp "data/aoc2018/day6.data"))

(defn calc-taken-area [plane]
  (->> plane
       (filter #(number? (val %)))
       (group-by val)
       (map #(vector (key %) (count (val %))))
       (into {})))

;; solve part 1
(comment
  (->> puzzle-input
       parse-coordinates
       #_[[0 0] [5 5] [8 8] [6 14] [13 1] [15 15]]
       init-plane
       (fixed-point expand)
       mark-boundary-values
       calc-taken-area
       vals
       (apply max)))


(defn distance [[ax ay] [bx by]]
  (+ (Math/abs (- ax bx))
     (Math/abs (- ay by))))

(defn total-distance [coordinates coordinate]
  (->> coordinates
       (map (partial distance coordinate))
       (reduce +)))

;; solve part 2
(comment
  (let [coordinates (->> puzzle-input
                         parse-coordinates)
        plane-coordinates (keys (init-plane coordinates))]
    (->> plane-coordinates
         (map (partial total-distance coordinates))
         (filter #(< % 10000))
         count)))
