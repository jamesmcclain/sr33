(ns mst.show
  (:require [quil.core :as q]))

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
