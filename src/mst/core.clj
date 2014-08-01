(ns mst.core
  (:require [clojure.set :as set]
            [clojure.java.io :as io]
            [mst.kdtree :as kdtree]
            [mst.graph_theory :as theory])
  (:use [clojure.tools.nrepl.server :only [start-server stop-server]])
  (:gen-class))

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

(if (empty? @(*state* :points))
  (load-data))

(defn -main []
  (print "Α -")
  (defonce ^:dynamic *repl-server* (start-server :port 4005))
  (println " Ω"))
