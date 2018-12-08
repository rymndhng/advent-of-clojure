(ns advent-2018.08
  "Parsing input into a tree and reducing on it.

  I first attempted this without building an intermediate tree first. checksum
  was easy to do.

  For the 2nd part, I wanted to generalize the first solution to take a
  transducer-ey function. I wanted to build a function that could navigate the
  structure and reduce over these values. This started to get really
  complicated.

  I finally decided to transform the input into tree using clojure maps. This is
  a more natural structure for me to work with. I'm not too familiar with
  functional parsing, and the implementation was a bit awkward figuring out the
  API of: Input -> [Tree, Remaining Input].

  The tricky part for me was combining both stack-based and linear recursion in
  one function. Mixing both types of state management was tricky to wrap my head
  around.

  Once I got the tree, I was able to use `reduce` to navigate through the tree
  for each part.
  "
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def data (->> (.split (.trim (slurp "src/advent_2018/08.input")) " ")
               (map #(Integer/parseInt %))))

;; purely functional node comsumption requires [output, remaining-nodes]
(defn as-tree
  [data]
  (let [[c-nodes-cnt m-nodes-cnt & rest-data] data]
    (if (zero? c-nodes-cnt)
      (let [[metadata rest-data] (split-at m-nodes-cnt rest-data)]
        [{:metadata metadata } rest-data])

      (loop [n         c-nodes-cnt
             children  []
             rest-data rest-data]
        (if (zero? n)
          (let [[metadata rest-data] (split-at m-nodes-cnt rest-data)]
            [{:children children
              :metadata metadata}
             rest-data])

          (let [[child rest-data'] (as-tree rest-data)]
            (recur (dec n)
                   (conj children child)
                   rest-data')))))))

(first (as-tree [0 1 99]))
{:metadata (99)}

(first (as-tree [1 1 0 1 99 2]))
{:children [{:metadata (99)}], :metadata (2)}

(first (as-tree [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2]))
{:children [{:metadata (10 11 12)} {:children [{:metadata (99)}], :metadata (2)}], :metadata (1 1 2)}

;; -- Part 1  ------------------------------------------------------------------
(defn checksum [{:keys [children metadata]}]
  (reduce #(+ %1 (checksum %2)) (apply + metadata) children))

(checksum (first (as-tree [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])))
138

(defn checksum-original
  "Original implementation that didn't use an intermediate tree for checksumming"
  ([data]
   (let [[x y & rst] data]
     (checksum-original 0 (list [x y]) rst)))
  ([acc stack data]
   ;; (println "acc" acc "stack" stack "data" data)
   (if (empty? stack)
     acc                               ;we must be done
     (let [[header & rest-stack] stack
           [c-nodes m-nodes]     header]
       (if (zero? c-nodes)
         (let [[metadata rest-data] (split-at m-nodes data)]
           (recur (apply + acc metadata)
                  rest-stack
                  rest-data))

         (let [[new-header rest-data] (split-at 2 data)]
           (recur acc
                  (conj rest-stack [(dec c-nodes) m-nodes] new-header)
                  rest-data)))))))

(checksum [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])
138

(checksum-original [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])
138

;; -- Part 1  ------------------------------------------------------------------
(checksum (first (as-tree data)))
41926

;; -- Part 2  ------------------------------------------------------------------
(defn node-value [{:keys [children metadata]}]
  (if (empty? children)
    (apply + metadata)
    (reduce #(+ %1 (node-value (get children (dec %2)))) 0 metadata)))

(node-value (first (as-tree [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])))
66

(node-value (first (as-tree data)))
24262
