(ns mst.graph_theory
  (:require [quil.core :as q]
            [clojure.set :as set]
            [mst.kdtree :as kdtree])
  (:use [clojure.data.priority-map]))

;; --------------------------------------------------

(defn local-nn [hood jitter]
  (let [index (:index (first hood))
        hood (drop 1 hood)
        radius (* jitter (:dist (first hood)))
        hood (map :index (filter #(< (:dist %) radius) hood))]
    (set (map #(hash-set index %) hood))))

(defn nn [points kdtree k jitter]
  (letfn [(point->hood [xyz] (kdtree/query xyz kdtree k))
          (hood->edges [hood] (local-nn hood jitter))]
    (map vec (apply set/union (map (comp hood->edges point->hood) points)))))

;; --------------------------------------------------

(defn local-prim [hood]
  (letfn [(make-edge [leaf1 leaf2]
            (let [edge (vector (get leaf1 :index) (get leaf2 :index))
                  dist (kdtree/dist (:xyz leaf1) (:xyz leaf2))]
              (vector edge dist)))
          (make-edges [leaf1 hood]
            (loop [hood hood edges (list)]
              (let [leaf2 (first hood)]
                (if (nil? leaf2)
                  edges
                  (recur (rest hood) (conj edges (make-edge leaf1 leaf2)))))))
          (make-queue []
            (loop [hood hood edges (list)]
              (let [leaf (first hood)
                    hood (rest hood)]
                (if (or (nil? leaf) (nil? hood))
                  (into (priority-map) edges)
                  (recur hood (concat edges (make-edges leaf hood)))))))]
    (let [Q (make-queue)
          [u v] (first (first Q))
          Q (pop Q)]
      ;; Build the minimum spanning tree by running through the queue
      ;; until it is exhausted.
      (loop [covered (hash-set u v)
             Q Q
             mst (list #{u v})]
        ;; Look through the queue until an edge that meets the minimum
        ;; spanning tree at exactly one vertex is found.
        (letfn [(find-touching-edge []
                  (loop [Q Q]
                    (let [uv (vec (first (first Q)))
                          [u v] uv]
                      (cond
                       ;; The queue is empty or a valid edge has been
                       ;; found, stop.
                       (or (or (nil? uv) (nil? u) (nil? v))
                           (or (and (covered u) (not (covered v)))
                               (and (not (covered u)) (covered v))))
                       uv
                       ;; An invalid edge has been found, continue.
                       :else (recur (pop Q))))))]
          (let [[u v] (find-touching-edge)]
            (if (or (nil? u) (nil? v))
              (set mst)
              (recur (conj covered u v) (dissoc Q #{u v}) (conj mst #{u v})))))))))

(defn prim [points kdtree k]
  (letfn [(point->edges [xyz] (local-prim (kdtree/query xyz kdtree k)))]
    (map vec (apply set/union (map point->edges points)))))
