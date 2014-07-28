(ns mst.prim
  (:require [quil.core :as q]
            [clojure.set :as set]))

(defn prim [hood]
  (let [n (count hood)
        uncovered (set (range n))
        edges (loop [i 0 j (inc 0) edges (vector)]
                (cond
                 (and (< i n) (< j n)) (recur i (inc j) (conj edges #{i j}))
                 (and (< i n) (>= j n)) (recur (inc i) (inc (inc i)) edges)
                 (>= i n) edges))
        decorate (fn [e]
                   (let [[a b] (vec e)
                         d (apply q/dist (concat (nth hood a) (nth hood b)))]
                     (vector d e)))
        edges (map second (sort #(< (first %1) (first %2)) (map decorate edges)))]
    (loop [uncovered (set/difference uncovered (first edges)) tree (vector (first edges))]
      (if (empty? uncovered)
                                        ; if done, return tree
        (map vec tree)
                                        ; otherwise, build tree
        (let [edge (first (filter #(= 1 (count (set/intersection uncovered %))) edges))]
          (if (empty? edge)
            (map vec tree)
            (recur (set/difference uncovered edge) (conj tree edge))))))))
