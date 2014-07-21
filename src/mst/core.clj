(ns mst.core
  (:require [quil.core :as q]
            [clojure.set :as set]))

(defn dist [x y]
  (Math/sqrt (reduce + (map #(* % %) (map - x y)))))

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
                         d (dist (hood a) (hood b))]
                     (vector d e)))
        edges (sort #(< (first %1) (first %2)) (map decorate edges))
        edges (map second edges)
        tree (loop [uncovered (set/difference uncovered (first edges)) tree (vector (first edges))]
               (if (empty? uncovered)
                                        ; if done, return tree
                 tree
                                        ; otherwise, build tree
                 (let [edge (first (filter #(= 1 (count (set/intersection uncovered %))) edges))]
                   (recur (set/difference uncovered edge) (conj tree edge)))))]
    (map vec tree)))

(defn donut? [x]
  (let [d (dist x [0.5 0.5])]
    (and (< d 0.5) (> d 0.4))))

(defn generate-point []
  (loop [pt [(rand) (rand)]]
    (if (not (donut? pt))
      (recur [(rand) (rand)])
      pt)))

(defn generate-hood [n]
  (loop [i n hood []]
    (if (not (zero? i))
      (recur (dec i) (conj hood (generate-point)))
      hood)))

(defn show-point [[x y]]
  (let [diam 10
        x (* x (q/width))
        y (* y (q/height))]
    (q/ellipse x y diam diam)))

(defn show-line [[x1 y1 x2 y2]]
  (let [x1 (* x1 (q/width))
        x2 (* x2 (q/width))
        y1 (* y1 (q/height))
        y2 (* y2 (q/height))]
    (q/line [x1 y1] [x2 y2])))

(defn setup []
  (q/smooth)
  (q/frame-rate 7.83)
  (q/background 255)
  
  (let [hood (generate-hood 100)]
                                        ; lines
    (q/stroke 255 0 0)
    (q/stroke-weight 7)
    (loop [tree (map #(concat (hood (first %)) (hood (second %))) (prim hood))]
      (if (not (empty? tree))
        (do
          (show-line (first tree))
          (recur (rest tree)))))        ; should be able to map, but can't
                                        ; points
    (q/stroke 0)
    (q/fill 0)
    (loop [hood hood]
      (if (not (empty? hood))
        (do
          (show-point (first hood))
          (recur (rest hood)))))))

(q/defsketch example
  :title "MST"
  :setup setup
  :size [800 600])
