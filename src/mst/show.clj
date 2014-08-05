(ns mst.show
  (:require [clojure.java.io :as io]
            [quil.core :as q]
            [mst.kdtree :as kdtree]
            [mst.graph_theory :as theory]))

(def ^:dynamic *state* {:points (atom nil) :squeezed-points (atom nil)
                        :mst (atom nil) :nn (atom nil) :kdtree (atom nil)
                        :projection (atom nil) :filename (atom "")})

(def ^:dynamic *angle* (atom 0))

(defn load-data
  ([] (load-data "resources/cone.data"))
  ([filename]
     (let [points (with-open [r (io/reader filename)]
                    (loop [points (vector) lines (line-seq r)]
                      (if (empty? lines)
                        points
                        (recur (conj points (read-string (first lines))) (rest lines)))))
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
               nn (theory/nn points kdtree 9 1.05)
               mst (remove (set nn) (theory/prim points kdtree 9))]
           (reset! (*state* :points) points)
           (reset! (*state* :squeezed-points) (map squeeze points))
           (reset! (*state* :mst) mst)
           (reset! (*state* :nn) nn)
           (reset! (*state* :kdtree) kdtree)
           (reset! (*state* :projection) projection)
           (reset! (*state* :filename) filename))))))

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
  (swap! *angle* #(mod (+ % 0.05) (* 4 Math/PI)))
  (let [hood (map @(*state* :projection) @(*state* :squeezed-points))
        mst @(*state* :mst)
        nn @(*state* :nn)]

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
