(ns advent-2018.07
  "Practise for:

  - worker scheduling in a functional implementation
  - modelling state machine and making transitions appropriately

  NOTE:
  - could have modelled the domain better to be \"tigheter\"
  - could have updated the deps-map as we go instead of maintaining a separate set
  "
  (:require [clojure.java.io :as io]
            [clojure.set]))

(defn parse-line [s]
  (vec (rest (re-matches #"Step (\w) must be finished before step (\w) can begin." s))))

(def steps (->> (io/reader "src/advent_2018/07.input")
                (line-seq)
                (map parse-line)))

(def deps-map (reduce (fn [acc [k v]] (update acc v conj k)) {} steps))

(def nodes (set
            (concat
             (map first steps)
             (map second steps))))

(defn breadth-walk [deps-map]
  (loop [path      []
         visited   #{}
         unvisited (set nodes)]
    (if (empty? unvisited)
      path
      (let [x (->> unvisited
                   (sort-by (fn [v] [(count (clojure.set/difference (set (get deps-map v)) visited))
                                     v]))
                   first)]
        (recur (conj path x)
               (conj visited x)
               (disj unvisited x))))))

;; we know the order
(apply str (breadth-walk deps-map))
"IJLFUVDACEHGRZPNKQWSBTMXOY"

;; -- Part 2  ------------------------------------------------------------------
(def *num-of-workers* 5)
(def *job-time-base* 60)

(defn new-system [deps-map]
  {:seconds -1
   :workers []
   :done []
   :queue (set (concat
                (map first steps)
                (map second steps)))
   :deps-map deps-map})

(defn update-completed-jobs [{:keys [workers] :as system}]
  (let [done-workers (group-by #(zero? (second %)) workers)]
    (-> system
        (update :done into (map first (get done-workers true)))
        (assoc :workers (get done-workers false)))))

(defn work [{:keys [workers] :as system}]
  (-> system
      (update :seconds inc)
      (update :workers (fn [workers]
                         (map #(update % 1 dec) workers)))))

(defn job-time [j]
  (+ *job-time-base*
     (- (Character/getNumericValue (.charAt j 0)) 9) ;compensate with -9 because A = 10
     ))

(defn next-job [{:keys [deps-map queue done]}]
  (->> queue
       (filter #(zero? (count (clojure.set/difference (set (get deps-map %))
                                                      (set done)))))
       (sort-by first)                  ;keep the next job algorithm as above
       first))

(defn schedule [{:keys [workers queue] :as system}]
  (if (or (<= *num-of-workers* (count workers))
          (empty? queue))
    system
    (if-let [job (next-job system)]
      (recur (-> system
                 (update :workers conj [job (job-time job)])
                 (update :queue disj job)))
      system)))

(defn tick [{:keys [workers queue] :as system}]
  (if (and (empty? workers)
           (empty? queue))
    system
    (-> system
        (update-completed-jobs)
        (schedule)
        (work))))

(comment

  (def steps (->> (io/reader "src/advent_2018/07.input")
                  (line-seq)
                  (map parse-line)))

  (def deps-map (reduce (fn [acc [k v]] (update acc v conj k)) {} steps))

  (def nodes (set
              (concat
               (map first steps)
               (map second steps))))

  (defn do-forever [v]
    (cons v (lazy-seq (do-forever (tick v)))))

  ;; IJLFUVDACEHGRZPNKQWSBTMXOY
  (nth (rest (do-forever (new-system deps-map))) 0)
  (nth (rest (do-forever (new-system deps-map))) 1)
  (nth (rest (do-forever (new-system deps-map))) 2)
  (nth (rest (do-forever (new-system deps-map))) 3)
  (nth (rest (do-forever (new-system deps-map))) 4)
  (nth (rest (do-forever (new-system deps-map))) 5)
  (nth (rest (do-forever (new-system deps-map))) 6)
  (nth (rest (do-forever (new-system deps-map))) 7)
  (nth (rest (do-forever (new-system deps-map))) 8)
  (nth (rest (do-forever (new-system deps-map))) 9)
  (nth (rest (do-forever (new-system deps-map))) 10)
  (nth (rest (do-forever (new-system deps-map))) 11)
  (nth (rest (do-forever (new-system deps-map))) 12)
  (nth (rest (do-forever (new-system deps-map))) 13)
  (nth (rest (do-forever (new-system deps-map))) 14)
  (nth (rest (do-forever (new-system deps-map))) 15)
  (nth (rest (do-forever (new-system deps-map))) 16)
  (nth (rest (do-forever (new-system deps-map))) 17)
  (nth (rest (do-forever (new-system deps-map))) 1068)

  (nth (rest (do-forever (new-system deps-map))) 1500)
  ;; answer
  {:seconds 1072,
   :workers (),
   :done ["I" "J" "L" "V" "F" "D" "U" "H" "A" "C" "E" "R" "G" "Z" "P" "N" "Q" "K" "W" "S" "B" "T" "M" "X" "O" "Y"],
   :queue #{},
   :deps-map {"T" ("Q" "K" "B" "F" "S" "U"), "K" ("J" "R" "P" "C" "E" "N"), "Q" ("P" "I" "G" "C"), "G" ("H" "I"), "M" ("H" "A" "V" "G" "D" "T" "Q" "B" "Z"), "S" ("P" "G" "C" "Z" "Q" "W" "V"), "Y" ("Z" "G" "S" "Q" "H" "W" "M" "X" "O" "T" "P"), "Z" ("D" "H" "C" "J" "A" "E" "F"), "H" ("F" "V"), "E" ("L" "D"), "R" ("F" "V"), "C" ("L" "D"), "F" ("L"), "B" ("G" "C" "N" "S" "Z" "Q"), "P" ("Z" "D" "E"), "U" ("I"), "O" ("N" "L" "U" "Q" "E" "T" "B" "C" "X" "M" "K"), "X" ("F" "E" "T" "K" "M" "B" "S" "H"), "N" ("P"), "A" ("I" "D" "J"), "W" ("D" "N" "C" "V" "R"), "D" ("V" "L")}}

  )
