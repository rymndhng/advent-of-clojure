(ns advent-2018.04
  "Concepts:

  - parsing
  - data enrichment for debugging
  - stateful event processing


  "
  (:require [clojure.java.io :as io])
  (:import [java.time.temporal ChronoUnit]))

(defn deserialize [input]
  (let [[_ day hour minute event] (re-matches #"\[(\d+-\d+-\d+) (\d+):(\d+)\] (.*)" input)]
    {:day    (java.time.LocalDate/parse day)
     :hour   (Integer/parseInt hour)
     :minute (Integer/parseInt minute)
     :event  event}))

(defn normalize-time [{:keys [day hour minute] :as input}]
  (assoc input
         :normal/day    (if (= hour 23)
                          (.plus day 1 ChronoUnit/DAYS)
                          day)

         :normal/minute (if (= hour 23)
                          0
                          minute)))

(defn enrich-event [{:keys [event] :as input}]
  (cond
    (= event "wakes up")
    (assoc input :action :wakeup)

    (= event "falls asleep")
    (assoc input :action :sleep)

    :else
    (let [[_ guard] (re-matches #"Guard #(\d+) begins shift" event)]
      (assoc input :action :begin-shift :guard guard))))

(def events (->> (io/reader "src/advent_2018/04.input")
                 (line-seq)
                 (sort)
                 (map (comp enrich-event normalize-time deserialize))))

(def events-by-date (partition-by :normal/day events))

(comment
  (nth events-by-date 5))

(defn begin-shift [begin-shift-event]
  {:date (:normal/day begin-shift-event)
   :guard (:guard begin-shift-event)
   :schedule (into [] (repeat 60 0))
   :state :awake})

(defn update-sleep [schedule start end]
  (if (= start end)
    schedule
    (recur (update schedule start inc) (inc start) end)))

(count (update-sleep (into [] (repeat 60 0)) 1 60))

(nth (into [] (repeat 60 0)) 60)

(defn time-asleep
  ([events]
   (time-asleep nil events))
  ([state [{:keys [action] :as x} & xs]]
   (cond
     (nil? x)
     state

     (= action :begin-shift)
     (recur (begin-shift x) xs)

     (= action :wakeup)
     (do (assert (= :sleeping (:state state)))
         (recur (-> state
                    (update :schedule update-sleep (:sleep-start state) (:normal/minute x))
                    (assoc :state :awake))
                xs))

     (= action :sleep)
     (do (assert (= :awake (:state state)))
         (recur (assoc state
                       :state :sleeping
                       :sleep-start (:normal/minute x))
                xs)))))

(defn safe-add [x y]
  (if (nil? x)
    y
    (+ x y)))

(defn count-time-asleep [acc guard]
  (update acc (:guard guard) safe-add (apply + (:schedule guard))))

(time-asleep (first events-by-date))


(def guards-total-time-asleep
  (->> events-by-date
       (map time-asleep)
       (reduce count-time-asleep {})))

(take 3 (sort-by (fn [[k v]] (- v)) guards-total-time-asleep))

'(["2663" 471] ["1093" 434] ["2917" 412])

;; let's use guard 2663
(map + [1 2 3] [4 5 6])

(->> events-by-date
     (map time-asleep)
     (filter #(= "2663" (:guard %)))
     (map :schedule)
     (reduce #(map + %1 %2))

     ;; sorting
     (map-indexed (fn [k v] (vector k v)))
     (sort-by #(- (second %))))

38
;; multiply 38 * 2663 => 101194

;; -- Part 2  ------------------------------------------------------------------
(defn most-minute-asleep [sleep-schedules]
  (->> sleep-schedules
       (reduce #(map + %1 %2))
       (map-indexed (fn [k v] (vector k v)))
       (sort-by #(- (second %)))
       first))

;; how to get the answer
(->> events-by-date
     (map time-asleep)
     (group-by :guard)
     (map (fn [[k v]]
            {:guard k
             :most-times-asleep (->> v
                                     (map :schedule)
                                     most-minute-asleep)}))
     (sort-by #(-> % :most-times-asleep second -))
     first)

{:guard "2917", :most-times-asleep [35 16]}

;; Part 2 answer
(* 2917 35)
;; 102095
