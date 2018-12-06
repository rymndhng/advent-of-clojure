(ns advent-2018.06
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

;; Helps look for reflection problems in hot loops b/c we're doing ~5 million computations
(set! *warn-on-reflection* true)

(def coordinates (->> (io/reader "src/advent_2018/06.input")
                     (line-seq)
                     (map #(string/split % #",\s+"))
                     (map (fn [[a b]]
                            [(Integer/parseInt a)
                             (Integer/parseInt b)]))))

;; 1. define bounds of system for iteration
;; 2. iterate through each box and figure out what the taxicab distance is for each box
;; 3. iterate through edges and remove the centres that touch any of them
;; 4. count the largest

(defn init-grid [coordinates]
  (let [xs (map first coordinates)
        ys (map second coordinates)]
    {:x-start (apply min xs)
     :y-start (apply min ys)
     :x-length (- (apply max xs) (apply min xs))
     :y-length (- (apply max ys) (apply min ys))
     :centers (into [] coordinates)     ;ensure they're numerically indexed
     :grid {}}))

;; fuck yes, typehinting made a few orders of magnitude improvement
(defn taxicab-distance [[ax ay] [bx by]]
  (+ (Math/abs ^Integer (- bx ax))
     (Math/abs ^Integer (- by ay))))

(defn each-point [{:keys [x-start y-start x-length y-length]}]
  (for [x (range x-start (+ x-start x-length))
        y (range y-start (+ y-start y-length))]
    [x y]))

(defn shortest-taxicab-distance [point centers]
  (let [[x y] (take 2 (sort-by #(taxicab-distance point %) centers))]
    (if (= (taxicab-distance point x)
           (taxicab-distance point y))
      (do (printf "loc: %s. tie between: %s %s\n" point x y)
          nil)
      (do (printf "loc: %s. closest by: %s\n" point x)
          x))))

(defn centers-with-infinite-progression [{:keys [x-start y-start x-length y-length grid] :as grid0}]
  (->> (each-point grid0)
       (filter (fn [[x y]]
                 (or (= x x-start) (= x (+ x-start x-length))
                     (= y y-start) (= y (+ y-start y-length)))))
       (map #(get grid %))
       (into #{})))

(defn update-grid [{:keys [centers] :as grid} point]
  (update grid :grid assoc point (shortest-taxicab-distance point centers)))

(comment

  (def grid (init-grid coordinates))

  (* (* (:x-length grid) (:y-length grid))
     (count (:centers grid)))
  4914000                                 ;this many iterations -- 5 million in a naive implementaion,

  (take 10 (each-point grid))


  ;; 10 seconds
  (time
   (def final-grid (reduce update-grid grid (each-point grid))))

  (def counts (-> (frequencies (map second (:grid final-grid)))
                  (dissoc nil)))



  (def counts-with-out-infinite-progression
    (reduce dissoc counts (centers-with-infinite-progression final-grid)))

  (count counts)
  (first (sort-by (fn [[k v]]  (- v)) counts-with-out-infinite-progression))


  ;; tests
  (shortest-taxicab-distance [0 0] [[0 2] [0 3] [3 0]])
  (shortest-taxicab-distance [2 2] [[3 3] [2 2] [1 1]])
  (shortest-taxicab-distance [2 2] [[3 3] [1 1]])
  )

;; -- Part 2  ------------------------------------------------------------------
(def *max-distance-sum* 10000)
(defn in-safe-region [point centers]
  (< (apply + (map #(taxicab-distance point %) centers))
      *max-distance-sum*))



(defn update-grid-in-safe-zone [{:keys [centers] :as grid} point]
  (update grid :grid assoc point (in-safe-region point centers)))

(comment

  (in-safe-region [0 0] [[0 999] [1 0]])
  true
  (in-safe-region [0 0] [[0 999] [1 1]])
  false

  (def safe-zone-final-grid (reduce update-grid-in-safe-zone grid (each-point grid)))

  (count (filter (fn [[k v]] (true? v)) (:grid safe-zone-final-grid)))
  36216

  )
