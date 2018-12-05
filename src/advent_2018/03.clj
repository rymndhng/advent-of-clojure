(ns advent-2018.03
  "Fundamentals:

  - stream processing
  - stateful event processing
  - data structure fundamentals & design
  - higher order functions

  "
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(defn deserialize [claim-str]
  (let [[_ id left top wide tall]
        (re-matches #"#(\d+).* @ (\d+),(\d+): (\d+)x(\d+)" claim-str)]
    {:id id
     :left (read-string left)
     :top (read-string top)
     :wide (read-string wide)
     :tall (read-string tall)}))

(def claims (->> (io/reader "src/advent_2018/03.input")
                (line-seq)
                (map deserialize)))

(defn positional-pieces [{:keys [left top wide tall]}]
  (for [wide-offset (range wide)
        tall-offset (range tall)]
    [(+ left wide-offset)
     (+ top tall-offset)]))

(defn safe-inc [n]
  (if (nil? n) 1 (inc n)))

(defn update-overlaps
  [acc claim]
  (reduce #(update %1 %2 safe-inc) acc (positional-pieces claim)))

(def overlaps
  (time
   (->> claims
        (reduce update-overlaps {}))))

(def overlapping-count
  (->> overlaps
       (filter (fn [[k v]] (> v 1)))
       (count)))

overlapping-count
113966

;; -- part 2  ------------------------------------------------------------------
(defn non-overlapping? [overlaps claim]
  (->> (positional-pieces claim)
       (map overlaps)
       (every? #(= 1 %1))))

(filter #(non-overlapping? overlaps %1) claims)
({:id "235", :left 780, :top 737, :wide 14, :tall 27})
