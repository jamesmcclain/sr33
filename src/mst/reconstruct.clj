(ns mst.reconstruct
  (:require [mst.kdtree :as kdtree]
            [mst.graph_theory :as theory]))

;; Not used, there to facilitate REPL interaction.
(defn compute-adjlist [points edges]
  (letfn [(register-edge [edge adjlist]
            (let [[a b] (vec edge)
                  a-list (conj (get adjlist a) b)
                  b-list (conj (get adjlist b) a)]
              (assoc adjlist a a-list b b-list)))]
    (loop [adjlist (vec (repeat (count points) #{})) edges edges]
      (let [edge (first edges)
            edges (rest edges)]
        (if (empty? edge)
          adjlist
          (recur (register-edge edge adjlist) edges))))))

;; Not used, there to facilitate for REPL interaction.
(defn compute-edges
  ([points k]
     (let [kdtree (kdtree/build points)]
       (compute-edges points kdtree k)))
  ([points kdtree k]
     (theory/MST points kdtree k)))

;; Compute the fan at u.
(defn local-fan [u adju edges hood]
  (letfn [(face [a b]
            (if (edges #{a b})
              ;; found a triangle, only report if u is smaller than a and b
              (if (and (< u a) (< u b)) (list a u b) nil)
              ;; did not find a triangle, look for a quad
              (loop [hood (disj (set hood) a u b)]
                (let [v (first hood)]
                  (cond
                   ;; found a quad at u
                   (and (not (nil? v))
                        (not (nil? (edges #{a v})))
                        (not (nil? (edges #{b v})))
                        (< u a) (< u b) (< u v))
                   (list a u b v)
                   ;; did not find quad
                   (not (nil? v)) (recur (rest hood))
                   (nil? v) nil)))))]
    (loop [a-list (seq adju) b-list (rest adju) cycles (list)]
      (let [a (first a-list)
            b (first b-list)]
        (cond
         ;; see if a-u-b is part of a face
         (and (not (nil? a)) (not (nil? b)))
         (let [cycle (face a b)
               cycles (if (not (nil? cycle))
                        (conj cycles cycle)
                        cycles)]
           (recur a-list (drop 1 b-list) cycles))
         ;; advance a
         (and (not (nil? a)) (nil? b))
         (recur (drop 1 a-list) (drop 2 a-list) cycles)
         ;; done
         (nil? a)
         cycles)))))

;; Recover a surface from an organized collection of point samples on
;; its surface by computing a subset of the Delaunay Triangulation
;; (that is, assuming the sample conditions hold).
(defn reconstruct
  ;; Points and k
  ([points k]
     (reconstruct points (kdtree/build points) k))

  ;; Points, kdtree, k
  ([points kdtree k]
     (reconstruct points kdtree (theory/RNG points kdtree k) k))
  
  ;; Points, kdtree, edges, k
  ([points kdtree edges k]
     (letfn [(register-edge [edge adjlist]
               (let [[a b] (vec edge)
                     a-list (conj (get adjlist a) b)
                     b-list (conj (get adjlist b) a)]
                 (assoc adjlist a a-list b b-list)))
             (compute-adjlist []
               (loop [adjlist (vec (repeat (count points) #{})) edges edges]
                 (let [edge (first edges)
                       edges (rest edges)]
                   (if (empty? edge)
                     adjlist
                     (recur (register-edge edge adjlist) edges)))))]
       (reconstruct points kdtree edges (compute-adjlist) k)))

  ;; Points, kdtree, edges, adjacency list, k
  ([points kdtree edges adjlist k]
     (letfn [(u-to-fan [u]
               (let [adju (get adjlist u)
                     hood (map :index (kdtree/query (get points u) kdtree (+ k 7)))]
                 (local-fan u adju edges hood)))]
       (apply concat (pmap u-to-fan (range (count points)))))))
