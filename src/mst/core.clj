(ns mst.core
  (:require [quil.core :as q]
            [clojure.set :as set])
  (:use [mst.prim]
        [mst.show]))

(defn donut? [x]
  (let [d (apply q/dist 0.5 0.5 x)]
    (and (< d 0.50) (> d 0.33))))

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

(defn setup []
  (q/smooth)
  (q/frame-rate 7.83)
  (q/background 255)
  
  (let [hood (generate-hood 100)]
    (q/stroke 255 0 0) ; lines
    (q/stroke-weight 3)
    (doseq [edge (map #(concat (hood (first %)) (hood (second %))) (prim hood))]
      (show-line edge))
    (q/stroke 0)       ; points
    (q/fill 0)
    (q/stroke-weight 3)
    (doseq [point hood]
      (show-point point))))

(q/defsketch example
  :title "MST"
  :setup setup
  :size [500 500])
