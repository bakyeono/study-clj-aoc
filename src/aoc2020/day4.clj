;; https://adventofcode.com/2020/day/4$part2
(ns aoc2020.day4
  (:require [clojure.spec.alpha :as spec])
  (:require [clojure.string :as string]))

(def puzzle-input (slurp "data/aoc2020/day4.data"))

(defn split-passport-source-chunks [passport-source-chunks]
  (string/split passport-source-chunks #"\n\n"))

(defn parse-int-or [default s]
  (try
    (Integer/parseInt s)
    (catch Exception e default)))

(def parse-int-or-nil (partial parse-int-or nil))

(defn coerce-passport-detail [[k v]]
  (let [k (keyword k)]
    [k
     (cond (#{:ecl} k) (keyword v)
           (#{:byr :iyr :eyr} k) (parse-int-or-nil v)
           (#{:hgt} k) (let [matches (re-matches #"^(\d{2,3})(cm|in)$" v)
                             value (parse-int-or-nil (nth matches 1))
                             unit (keyword (nth matches 2))]
                         {:value value :unit unit})
           (#{:hcl :pid :cid} k) v)]))

(defn parse-passport [passport-source]
  (->>
    (string/split passport-source #"[\s:]")
    (partition 2)
    (map coerce-passport-detail)
    (into {})))

(def passport-source (first (split-passport-source-chunks puzzle-input)))

(parse-passport passport-source)

(spec/def :form/four-digits #(re-matches #"\d{4}" %))
(spec/def :form/rgb-hash #(re-matches #"#[0-9a-f]{6}" %))

(spec/def :passport/byr (spec/int-in 1920 2003))
(spec/def :passport/iyr (spec/int-in 2010 2021))
(spec/def :passport/eyr (spec/int-in 2020 2031))
(spec/def :passport/hgt (fn [{:keys [value unit]}]
                          (and (#{:cm :in} unit)
                               (case unit :cm (<= 150 value 193)
                                          :in (<= 59 value 76)))))
(spec/def :passport/hcl :form/rgb-hash)
(spec/def :passport/ecl #{:amb :blu :brn :gry :grn :hzl :oth})
(spec/def :passport/pid #(re-matches #"\d{9}" %))
(spec/def :passport/cid (constantly true))

(spec/def :passport/passport
  (spec/keys :req-un [:passport/byr :passport/iyr :passport/eyr :passport/hgt :passport/hcl
                      :passport/ecl :passport/pid]
             :opt-un [:passport/cid]))

(defn solve [puzzle-input]
  (->> puzzle-input
       split-passport-source-chunks
       (map parse-passport)
       #_(filter #(spec/valid? :passport/passport %))
       #_count))

(defn explain [puzzle-input]
  (->> puzzle-input
       split-passport-source-chunks
       (map parse-passport)
       (map #(spec/explain :passport/passport %))))

(solve puzzle-input)

;; spec/int-in
;; int-in을 사용하려면 먼저 digits 을 numbers 로 변경해야 한다. parse 전에 validation 을 먼저 해야 할 것 같은데...
