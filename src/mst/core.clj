(ns mst.core
  (:require [quil.core :as q]
            [clojure.set :as set]
            [mst.kdtree :as kdtree]
            [mst.graph_theory :as theory])
  (:use [mst.show])
  (:gen-class))

(def ^:dynamic *state* {:points (atom nil) :squeezed-points (atom nil)
                        :mst (atom nil) :nn (atom nil) :kdtree (atom nil)
                        :projection (atom nil) :filename (atom "")})
(def ^:dynamic *angle* (atom 0))

(defn load-data
  ([] (load-data "resources/cone.data"))
  ([filename]
     (let [points (load-file filename)
           xmin (reduce min (map #(nth % 0) points))
           xmax (reduce max (map #(nth % 0) points))
           xrange (- xmax xmin)
           ymin (reduce min (map #(nth % 1) points))
           ymax (reduce max (map #(nth % 1) points))
           yrange (- ymax ymin)
           zmin (reduce min (map #(nth % 2) points))
           zmax (reduce max (map #(nth % 2) points))
           zrange (- zmax zmin)]
       (letfn [(squeeze [[a b c]]
                 [(/ (- a xmin) xrange) (/ (- ymax b) yrange) (/ (- c zmin) zrange)])
               (projection [[a b c]]
                 (let [angle (/ @*angle* 2)
                       [a b c] (vec (map #(- % 0.5) [a b c]))
                       [a b c] [(+ (* a (Math/cos angle)) (* c (Math/sin angle)))
                                b
                                (- (* c (Math/cos angle)) (* a (Math/sin angle)))]
                       [a b c] [a
                                (- (* b (Math/cos @*angle*)) (* c (Math/sin @*angle*)))
                                (+ (* b (Math/sin @*angle*)) (* c (Math/cos @*angle*)))]
                       [a b c] (vec (map #(+ % 0.5) [a b c]))]
                   [a b]))]
         (let [kdtree (kdtree/build points)
               mst (theory/prim points)
               nn (remove (set mst) (theory/nn-1 points kdtree))]
           (reset! (*state* :points) points)
           (reset! (*state* :squeezed-points) (map squeeze points))
           (reset! (*state* :mst) mst)
           (reset! (*state* :nn) nn)
           (reset! (*state* :kdtree) kdtree)
           (reset! (*state* :projection) projection)
           (reset! (*state* :filename) filename))))))

(if (empty? @(*state* :points))
  (load-data))

(defn setup []
  (q/frame-rate 5)
  (q/background 255))

(defn draw []
  (q/background 255)
  (q/stroke-weight 3)
  (swap! *angle* #(mod (+ % 0.01) (* 4 Math/PI)))
  (let [hood (map @(*state* :projection) @(*state* :squeezed-points))
        mst @(*state* :mst)
        nn @(*state* :nn)]

    ;; (q/stroke-weight 3) ; lines
    (q/stroke 0 0 255)
    (doseq [edge (map #(concat (nth hood (first %)) (nth hood (second %))) nn)]
      (show-line edge))
    (q/stroke 255 0 0)
    (doseq [edge (map #(concat (nth hood (first %)) (nth hood (second %))) mst)]
      (show-line edge))

    ;; (q/stroke-weight 3) ; points
    (q/stroke 0)
    (q/fill 0)
    (doseq [point hood]
      (show-point point))))

(defn -main []
  (q/defsketch example
    :title "MST"
    :setup setup
    :draw draw
    :size [800 800]
    :renderer :opengl))
