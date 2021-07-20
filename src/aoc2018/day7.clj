;; https://adventofcode.com/2018/day/7
(ns aoc2018.day7
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(def rule-pattern #"Step ([A-Z]) must be finished before step ([A-Z]) can begin.")

(defn parse-rule [rule-string]
  (let [[_ pre post] (re-matches rule-pattern rule-string)]
    {:pre (keyword pre) :post (keyword post)}))

(defn parse-rules [input]
  (->> input
       string/split-lines
       (map parse-rule)))

(defn prepare-todo
  "작업 규칙 시퀀스와 작업 소요시간 함수로 할 일 시퀀스를 만든다.

  ## 인자
  * rules: 작업의 선후 관계를 나타내는 규칙의 시퀀스. 예: [{:pre :A :post :B} {:pre :B :post :C}]
  * work->seconds: 각 작업에 소요되는 시간을 구하는 함수. 예: #{:A 1 :B 2 :C 3}

  ## 반환
  * 할 일을 담은 시퀀스. 예: [{:work :A :pre #{:B :C} :seconds 1}, ...]

  ## 예
  (prepare-todo [{:pre :A :post :B} {:pre :C :post :B}]
                {:A 1 :B 2 :C 3})
  => ({:work :A :seconds 1 :pre #{}}
      {:work :B :seconds 2 :pre #{:A :C}}
      {:work :C :seconds 3 :pre #{}})
  "
  [rules work->seconds]
  (let [works (set/union (set (map :pre rules))
                         (set (map :post rules)))
        get-pre-works-of (fn [work]
                           (->> rules
                                (filter (fn [rule] (= (:post rule) work)))
                                (map :pre)
                                set))
        make-todo-item (fn [work]
                         {:work work
                          :seconds (work->seconds work)
                          :pre (get-pre-works-of work)})]
    (map make-todo-item works)))

(def default-worker {:available-at 0 :work nil})
(defn prepare-workers [n]
  (into [] (take n (repeat default-worker))))

(defn init-plan [rules work->seconds number-of-workers]
  "작업규칙, 작업->시간 함수, 워커의 수를 입력받아, 작업 초기 단계를 설정한다.

  ## 인자
  * rule: 작업의 선후 관계를 나타내는 규칙의 시퀀스. 예: [{:pre :A :post :B} {:pre :B :post :C}]
  * work->seconds: 각 작업에 소요되는 시간을 구하는 함수. 예: #{:A 1 :B 2 :C 3}
  * number-of-workers: 워커의 수. 예: 1

  ## 반환
  * 작업 단계: 다음 키를 갖는 해시 맵
    * :at      작업 단계의 시각 (초)
    * :todo    작업 단계에서 남은 할 일의 시퀀스
    * :done    완료한 작업의 벡터
    * :workers 작업을 처리하는 워커의 시퀀스

  ## 예
  (init-plan [{:pre :A :post :B} {:pre :C :post :B}]
             {:A 1 :B 2 :C 3}
             2)
  => {:at -1
      :todo ({:work :A :seconds 1 :pre #{}}
             {:work :B :seconds 2 :pre #{:A :C}}
             {:work :C :seconds 3 :pre #{}})
      :done []
      :workers [{:available-at 0 :work nil}
                {:available-at 0 :work nil}]}
  "
  {:at -1
   :todo (prepare-todo rules work->seconds)
   :done []
   :workers (prepare-workers number-of-workers)})

(defn forward-to-next-finishing-work
  "at을 워커에 할당된 작업 중 가장 먼저 완료할 수 있는 작업의 완료 시간으로 옮긴다."
  [{:as stage :keys [at todo done workers]}]
  (assoc stage :at (->> workers
                        (map :available-at)
                        (filter #(< at %))
                        (apply min))))

(defn add-done-works-into-done
  "현재 at 기준, 완료한 작업을 done 에 추가한다."
  [{:as stage :keys [at todo done workers]}]
  (assoc stage :done (->> workers
                          (filter #(and (= at (:available-at %))
                                        (:work %)))
                          (map :work)
                          (into done))))

(defn remove-met-prerequisites-from-todo
  "각 할 일의 선행 조건 작업에서 완료된 작업을 제외한다."
  [{:as stage :keys [at todo done workers]}]
  (assoc stage :todo (->> todo
                          (map (fn [todo-item]
                                 (update todo-item :pre #(apply disj % done)))))))

(defn allocate-next-works-to-available-workers
  "여유 워커(들)에 다음에 할 작업(들)을 할당한다."
  [{:as stage :keys [at todo done workers]}]
  (let [available-todo-items (->> todo
                                  (filter (comp empty? :pre))
                                  (sort-by :work))
        available-slots (->> (range (count workers))
                             (filter #(<= (:available-at (nth workers %)) at)))
        todo-items-and-slots (map vector available-todo-items available-slots)
        allocate (fn [workers [{work :work seconds :seconds} slot]]
                   (assoc workers slot {:work work :available-at (+ at seconds)}))]
    (assoc stage :workers (reduce allocate workers todo-items-and-slots))))

(defn remove-allocated-works-from-todo
  "할 일에서 워커에 할당된 작업을 제외한다."
  [{:as stage :keys [at todo done workers]}]
  (let [allocated? (set (map :work workers))]
    (assoc stage :todo (->> todo
                            (filter (comp (complement allocated?) :work))))))

(defn next-stage
  "이전 작업 단계를 입력받아 다음 단계로 변환한다.

  ## 인자와 반환 (동일 형식)
  * 작업 단계: 다음 키를 갖는 해시 맵
    * :at      작업 단계의 시각 (초)
    * :todo    작업 단계에서 남은 할 일의 시퀀스
    * :done    완료한 작업의 벡터
    * :workers 작업을 처리하는 워커의 시퀀스

  ## 처리
  * 작업 단계의 상태가 다음 단계의 상태로 변환된다.
    * :at      워커가 처리에 할당된 작업 중 가장 먼저 완료할 수 있는 작업의 완료 시간으로 이동
    * :todo    (새 at 기준) 완료한 작업을 각 작업의 선행 조건 작업에서 제거하고, 새로 할당된 작업을 제거
    * :done    (새 at 기준) 완료한 작업을 추가
    * :workers (새 at 기준) 작업을 처리하고 있지 않은 워커에 다음 처리할 작업을 할당

  ## 예
  (next-stage {:at 50
               :todo ({:work :C :seconds 20 :pre #{:B}})
               :done [:A]
               :workers [{:work :B :available-at 100}]})
  => {:at 100
      :todo ()
      :done [:A :B]
      :workers [{:work :C, :available-at 120}]}
  "
  [{:as stage :keys [at todo done workers]}]
  (->> stage
       forward-to-next-finishing-work            ; 다음 최선 작업 완료 시간으로 이동
       add-done-works-into-done                  ; 완료된 작업을 갱신
       remove-met-prerequisites-from-todo        ; 각 할 일의 선행 조건 작업에서 완료된 작업을 제외
       allocate-next-works-to-available-workers  ; 여유 워커에 다음 작업을 할당
       remove-allocated-works-from-todo))        ; 할 일에서 할당된 작업을 제외


(defn done? [{:keys [at todo workers]}]
  "작업 단계를 입력받아 모든 작업이 완료되었는지 판단한다."
  (and (empty? todo)
       (empty? (filter #(< at (:available-at %))
                       workers))))

(defn last-stage
  "작업 단계 루프(iterate next-stage (init-plan ...))를 입력받아 마지막 단계를 반환한다."
  [stage-sequence]
  (->> stage-sequence
       (drop-while (complement done?))
       first))

(def keyword->character (comp second str))

(defn ascii-keyword->number
  "ASCII 코드 하나로 이루어진 키워드를 입력받아 그에 대응하는 부호를 반환한다.
  base로 조정치를 설정할 수 있다.
  예를 들어 :A 는 ASCII 코드에서 65이지만, 밑값을 -64로 설정하면 1이 반환된다."
  [base ascii-keyword]
  (+ base
     (int (keyword->character ascii-keyword))))

(defn character-keywords->string
  [character-keywords]
  (->> character-keywords
       (map keyword->character)
       (reduce str)))

(def puzzle-input (slurp "data/aoc2018/day7.data"))

;; solve part 1
(->> (init-plan (parse-rules puzzle-input)
                (partial ascii-keyword->number -64)
                1)
     (iterate next-stage)
     last-stage
     :done
     character-keywords->string)

;; solve part 2
(->> (init-plan (parse-rules puzzle-input)
                (partial ascii-keyword->number -4)
                5)
     (iterate next-stage)
     last-stage
     :at)
