(ns advent-2018.05
  (:require [clojure.java.io :as io]))

(def input (.trim (slurp "src/advent_2018/05.input")))

(defn reaction? [x y]
  (and (not= x y)
       (= (java.lang.Character/toLowerCase x)
          (java.lang.Character/toLowerCase y))))

(defn backout [x]
  (if (= 0 x) x (dec x)))

;; this is a little ridiculous, so we're going to use a mutable list
(defn alchemical-reduction-functional
  ([xs]
   (alchemical-reduction-functional 0 (into [] xs)))
  ([pos xs]
   (cond
     (= pos (dec (count xs)))
     (apply str xs) ;we're reduced

     (apply reaction? (take 2 (nthrest xs pos)))
     (recur (backout pos)
            (into (subvec xs 0 pos)
                  (subvec xs (+ 2 pos))))

     :else
     (recur (inc pos) xs))))

(defn alchemical-reduction-arraylist
  ([xs]
   (alchemical-reduction-arraylist 0 (java.util.ArrayList. (into [] xs))))
  ([pos ^java.util.ArrayList xs]
   (cond
     (empty? xs)
     ""

     (= pos (dec (count xs)))
     (apply str xs) ;we're reduced

     (reaction? (nth xs pos) (nth xs (inc pos)))
     (do
       (printf "At pos:%s removing: %s %s\n" pos (nth xs pos) (nth xs (inc pos)))
       (.remove xs (int pos))
       (.remove xs (int pos))               ;remove from the same pos twice b/c side-effecting
       (recur (backout pos) xs))

     :else
     (recur (inc pos) xs))))

(comment
  ;; this functional approach was too slow, so I rewrote it using an arraylist
  (time
   (count (alchemical-reduction-functional input))))


(time
 (with-open [w (io/writer "lol")]
   (binding [*out* w]
     (def result (alchemical-reduction-arraylist input)))))

(spit "lolresult" result)
;; 872 ms

(count result)
9562


;; -- Part 2  ------------------------------------------------------------------
(for [char (map char (range (int \a) (inc (int \z))))
      :let [trimmed-input (-> input
                             (.replace (str char) "")
                             (.replace (str (java.lang.Character/toUpperCase char))  ""))]]
  (count (alchemical-reduction-arraylist trimmed-input)))

(def optimal-result *1)

(first (sort optimal-result))
4934
