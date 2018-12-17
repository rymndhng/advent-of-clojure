(ns advent-2018.11)

(defn hundredths [x]
  (let [hundredths-digit (-> x
                             str
                             reverse
                             (nth 2))]
    (if hundredths-digit
      (read-string (str hundredths-digit))
      0)))

(mod 253 100)

(defn power-cell [serial x y]
  (let [rack-id     (+ 10 x)
        power-level (* rack-id (+ serial (* rack-id y)))]
    (- (hundredths power-level) 5)))

(def power-cell (memoize power-cell))

(defn total-power [serial x y size]
  (apply + (for [x' (range x (+ size x))
                 y' (range y (+ size y))]
             (power-cell serial x' y'))))

(defn max-power [serial size]
  (first (sort-by (fn [[x y]] (- y))
                  (for [x    (range 1 (- 301 size))
                        y    (range 1 (- 301 size))]
                    [[x y size] (total-power serial x y size)]))))

;; part 1
(first (max-power 4151 [3]))
[[20 46 3] 30]

(first (max-power 4151 [9]))
[[235 70 9] 132]

(first (max-power 4151 [10]))
[[234 69 10] 145]

(first (max-power 4151 [11]))
[[234 68 11] 146]

(first (max-power 4151 [12]))
[[233 63 12] 141]

(first (max-power 4151 [13]))
[[231 66 13] 149]

(first (max-power 4151 (range 14 24)))

(doseq [n (range 14 24)]
  (println (max-power 4151 n)))

;; correct answer
;; [[231 65 14] 158]
