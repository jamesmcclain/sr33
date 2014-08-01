(ns mst.show
  (:require [quil.core :as q]))

(defn setup []
  (q/frame-rate 20)
  (q/background 255))

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

(defn draw []
  (q/background 255)
  (q/stroke-weight 3)
  (swap! mst.core/*angle* #(mod (+ % 0.05) (* 4 Math/PI)))
  (let [hood (map @(mst.core/*state* :projection) @(mst.core/*state* :squeezed-points))
        mst @(mst.core/*state* :mst)
        nn @(mst.core/*state* :nn)]

    (q/stroke-weight 3)

    (q/stroke 0 0 255) ; lines
    (doseq [edge (map #(concat (nth hood (first %)) (nth hood (second %))) nn)]
      (show-line edge))
    (q/stroke 255 0 0)
    (doseq [edge (map #(concat (nth hood (first %)) (nth hood (second %))) mst)]
      (show-line edge))

    (q/stroke 0) ; points
    (q/fill 0)
    (doseq [point hood]
      (show-point point))))

(defn show []
  (q/defsketch example
    :title "MST"
    :setup setup
    :draw draw
    :size [600 600]
    :renderer :opengl))
