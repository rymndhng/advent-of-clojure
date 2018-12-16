(ns advent-2018.09
  "Practise modelling things in Clojure using maps.

  I originally attempted to solution this using a sequence and using the first
  element to represent the current marble.

  This solution was efficieint in the number of variables I had to capture.

  Unfortunately, it was too slow for rotating the sequence, because it's working
  with a sequence, which is O(n) ;/

  I wanted to use a doubly linked list to have fast insert/removals/traversals.
  Unfortunately the classic doubly linked list is not easy to implement in a
  immutable way. Fortunately, you can simulate the same characteristics using a
  map of {value -> [prev-value, next-value]}.
  "
  )

;; a naive sequence is too inefficient for large rotations,
;; instead simulate a linked list using a map
(defn init-game [num-players]
  {:players         (vec (repeat num-players 0))
   :marbles         {0 [0 0]}
   :current-player  0
   :current-marble  0
   :next-marble-num 1})

(defn rotate-coll [coll n]
  (let [[xs ys] (split-at n coll)]
    (-> []
        (into ys)
        (into xs))))

;; walking 5000 x 7 times seems like a really bad idea
(defn pull-7th-prev-marble [marbles]
  (let [[x & xs] (-> marbles reverse (rotate-coll 8))]
    [x (reverse xs)]))

;; update the pseudo linked list marble magic
(defn insert-next-marble [{:keys [marbles current-marble next-marble-num] :as state}]
  (let [prev    (next-marble marbles current-marble 1)
        next    (next-marble marbles current-marble 2)
        marbles (-> marbles
                    (assoc next-marble-num [prev next])
                    (assoc-in [prev 1] next-marble-num)
                    (assoc-in [next 0] next-marble-num))]
    (-> state
        (assoc :marbles marbles)
        (assoc :current-marble next-marble-num)
        (assoc :next-marble-num (inc next-marble-num)))))

(-> (insert-next-marble {:marbles {0 [0 0]} :current-marble 0 :next-marble-num 1})
    (insert-next-marble))
{:marbles {0 [1 2], 1 [2 0], 2 [0 1]}, :current-marble 2, :next-marble-num 3}

(-> (insert-next-marble {:marbles {0 [0 0]} :current-marble 0 :next-marble-num 1})
    (insert-next-marble)
    (insert-next-marble)
    (insert-next-marble))
{:marbles {0 [3 4], 1 [2 3], 2 [4 1], 3 [1 0], 4 [0 2]}, :current-marble 4, :next-marble-num 5}


(defn remove-marble [marbles marble]
  (let [[prev next] (get marbles marble)]
    (-> marbles
        (dissoc marble)
        (assoc-in [prev 1] next)
        (assoc-in [next 0] prev))))

(defn prev-marble
  ([marbles current-marble]
   (prev-marble marbles current-marble 1))
  ([marbles current-marble n]
   (if (= 0 n)
     current-marble
     (recur marbles (get-in marbles [current-marble 0]) (dec n)))))

(defn next-marble
  ([marbles current-marble]
   (next-marble marbles current-marble 1))
  ([marbles current-marble n]
   (if (= 0 n)
     current-marble
     (recur marbles (get-in marbles [current-marble 1]) (dec n)))))

(defn inc-current-player-points [player-scores points]
  (update-in )

  (let [[x & xs] player-scores]
    (cons (+ points x) xs)))

(defn rotate-players [{:keys [current-player players] :as state}]
  (assoc state :current-player (mod (inc current-player) (count players))))

(defn maybe-consume-current-marble [{:keys [marbles current-marble current-player] :as state}]
  (if (not= 0 (mod current-marble 23))
    state
    (let [seventh-prev            (prev-marble marbles current-marble 9)
          [_ next-current-marble] (get marbles seventh-prev)]
      (-> state
          (update :marbles remove-marble current-marble)
          (update :marbles remove-marble seventh-prev)
          (update-in [:players current-player] + current-marble seventh-prev)
          (assoc :current-marble next-current-marble)))))


(defn turn [{:keys [next-marble] :as state}]
  (-> state
      (insert-next-marble)
      (maybe-consume-current-marble)
      (rotate-players)))

(nth (iterate turn (init-game 9)) 1)
{:players [0 0 0 0 0 0 0 0 0], :marbles {0 [1 1], 1 [0 0]}, :current-marble 1, :next-marble-num 2}

(nth (iterate turn (init-game 9)) 22)


(nth (iterate turn (init-game 9)) 23)
{:players [0 0 0 0 0 0 0 0 32], :marbles {0 [15 16], 7 [14 15], 20 [2 10], 1 [11 12], 4 [17 18], 15 [7 0], 21 [10 5], 13 [6 3], 22 [5 11], 6 [12 13], 17 [8 4], 3 [13 14], 12 [1 6], 2 [19 20], 19 [18 2], 11 [22 1], 5 [21 22], 14 [3 7], 16 [0 8], 10 [20 21], 18 [4 19], 8 [16 17]}, :current-marble 19, :next-marble-num 24}

(comment
  (apply max (:players (nth (iterate turn (init-game 10)) 1618)))
  8317

  (apply max (:players (nth (iterate turn (init-game 13)) 7999)))
  146373

  (apply max (:players (nth (iterate turn (init-game 17)) 1104)))
  2764

  (apply max (:players (nth (iterate turn (init-game 21)) 6111)))
  54718

  (apply max (:players (nth (iterate turn (init-game 30)) 5807)))
  37305

  ;; exercise. how slow is this? no doubly linked list,
  ;; woohoo, only took ~1 seconds!
  (apply max (:players (nth (iterate turn (init-game 459)) 72103)))
  388131

  ;; part 2: took 90 seconds, fairly linear scaling :)
  (time
   (apply max (:players (nth (iterate turn (init-game 459)) 7210300))))
  3239376988


  ;; not good enough, need to use a different data structure lol

  )
