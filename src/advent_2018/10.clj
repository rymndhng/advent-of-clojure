(ns advent-2018.10
  (:require [clojure.java.io :as io]))

(defn parse-line [s]
  (let [[_ x y vx vy] (re-matches #"position=<(.*),(.*)> velocity=<(.*),(.*)>" s)]
    {:x  (read-string x)
     :y  (read-string y)
     :vx (read-string vx)
     :vy (read-string vy)}))

(def input (->> (io/reader "src/advent_2018/10.input")
               (line-seq)
               (map parse-line)))

(defn update-point [point time]
  (-> point
      (update :x + (* time (:vx point)))
      (update :y + (* time (:vy point)))))

(defn downsample-point [{:keys [x y]}]
  [(Math/floor (/ x 1)) (Math/floor (/ y 1))])

(defn render [points]
  (let [lookup (into (sorted-set) (map downsample-point) points)]
    (with-open [writer (io/writer (io/file "10.out") :append? false)]
      (binding [*out* writer]
        (doseq [y (range 0 500)
                x (range 0 500)]
          (do
            (if (contains? lookup [x y])
              (print "#")
              (print " "))
            (if (= x 199)
              (printf "\n"))))))))

(defn at-time [points time]
  (map #(update-point % time) points))

(last (range -500 500))

(into (sorted-set) (map downsample-point) input)

(render (at-time input 10285))
                                        ;35 -> 45

;; this was really hacky trial & error and looking through an editor
(render (at-time input 10243))

(take 10 (into (sorted-set) (map downsample-point)  (at-time input 10238)))
(update-point (first input) 1)


(count input)
390
