(ns advent-2018.02
  (:require [clojure.java.io :as io]))

(def input (line-seq (io/reader "src/advent_2018/02.input")))

(def partial-checksums (for [ids  input
                             :let [frequencies (frequencies ids)
                                   with-twos   (filter (fn [[k v]] (= v 2)) frequencies)
                                   with-threes (filter (fn [[k v]] (= v 3)) frequencies)]]
                         [(count with-twos)
                          (count with-threes)]))

(take 10 partial-checksums)
([3 0] [2 0] [3 0] [3 0] [3 0] [1 0] [3 0] [1 1] [3 0] [3 0])

(def checksum (let [twos (->> (map first partial-checksums)
                            (filter pos?)
                            (count))
                    threes (->> (map second partial-checksums)
                            (filter pos?)
                            (count))]
                (* twos threes)))
checksum
4980

;; -- Edit Distance  -----------------------------------------------------------
;; goal: make it fast, without resorting to O(n^2) sorting
;; sort sequence first, and then build a number
(defn common-characters
  ([s1 s2]
   (common-characters [] s1 s2))
  ([acc [x & xs] [y & ys]]
   (cond
     (or (nil? x) (nil? y))
     acc

     (= x y)
     (recur (conj acc x) xs ys)

     (not= x y)
     (recur acc xs ys))))

(def first-correct-box-ids
  (reduce
   (fn [acc next]
     (if (= (count (common-characters acc next))
            (dec (count acc)))
       (reduced [acc next])
       next))
   ""
   (sort input)))

(apply str (apply common-characters first-correct-box-ids))
"qysdtrkloagnfozuwujmhrbvx"
