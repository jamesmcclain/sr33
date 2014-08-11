(ns mst.reconstruct
  (:require [clojure.set :as set]
            [mst.kdtree :as kdtree]
            [mst.graph_theory :as theory]))

;; --------------------------------------------------

;; Compute the edges in the graph that is the reconstructed surface.
(defn compute-edges
  ([points k]
     (let [kdtree (kdtree/build points)]
       (compute-edges points kdtree k)))
  ([points kdtree k]
     (theory/RNG points kdtree k)))

;; Compute the graph adjacency list.
(defn compute-adjlist [points edges]
  (letfn [(register-edge [edge adjlist]
            (let [[a b] (vec edge)
                  a-list (conj (get adjlist a) b)
                  b-list (conj (get adjlist b) a)]
              (assoc adjlist a a-list b b-list)))]
    (loop [adjlist (vec (repeat (count points) #{})) edges edges]
      (if (empty? edges)
        adjlist
        (recur (register-edge (first edges) adjlist) (rest edges))))))

;; --------------------------------------------------

(defn not-simple? [our-loop]
  (let [last-index (last our-loop)]
    (loop [our-loop (drop 1 (drop-last 1 our-loop))]
      (cond
       (empty? our-loop) false
       (= (first our-loop) last-index) true
       :else (recur (rest our-loop))))))

(defn closed? [our-loop]
  (= (first our-loop) (last our-loop)))

(defn loops-at-u [hood adjlist u]
  (loop [stack (vector (vector u)) finished-loops (list)]
    (println stack)
    (if (empty? stack)
      ;; The stack is empty, so return the list of finished loops.
      finished-loops
      ;; Work on the loop on top of the stack.
      (let [current-loop (peek stack)]
        (cond
         ;; The loop is not simple, so discard it.
         (not-simple? current-loop)
         (do
           (println " 1:" current-loop)
           (recur (pop stack) finished-loops))
         ;; The loop is closed, transfer it to the list.
         (and (> (count current-loop) 1) (closed? current-loop))
         (do
           (println " 2:" current-loop)
           (recur (pop stack) (conj finished-loops current-loop)))
         ;; The loop is simple and open, so extend it.
         :else
         ;; This sexp returns the new stack:
         (recur
          (let [stack (pop stack)
                ;; The remove is to make sure that the loop contains only indices <= u.
                neighbors (remove #(< % u) (set/intersection (get adjlist (last current-loop)) hood))]
            (loop [neighbors neighbors stack stack]
              (if (empty? neighbors)
                stack
                (recur (rest neighbors) (conj stack (conj current-loop (first neighbors)))))))
          finished-loops))))))

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
     (reconstruct points kdtree (compute-edges points kdtree k) k))
  
  ;; Points, kdtree, edges, k
  ([points kdtree edges k]
     (reconstruct points kdtree edges (compute-adjlist points edges) k))

  ;; Points, kdtree, edges, adjacency list, k
  ([points kdtree edges adjlist k]
     (letfn [(u-to-fan [u]
               (let [adju (get adjlist u)
                     hood (map :index (kdtree/query (get points u) kdtree (+ k 7)))]
                 (local-fan u adju edges hood)))]
       (apply concat (pmap u-to-fan (range (count points)))))))
