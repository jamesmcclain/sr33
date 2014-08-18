(ns mst.reconstruct
  (:require [clojure.set :as set]
            [clojure.core.memoize :as memo]
            [mst.kdtree :as kdtree]
            [mst.graph_theory :as theory]))

(def ^:dynamic *max-cycle-size* 7)
(def ^:dynamic *Δr* 3)

;; Compute the graph adjacency list.
(defn compute-adjlist [n edges]
  (letfn [(register-edge [edge adjlist]
            (let [[a b] (vec edge)
                  a-list (conj (get adjlist a) b)
                  b-list (conj (get adjlist b) a)]
              (assoc adjlist a a-list b b-list)))]
    (loop [adjlist (vec (repeat n #{})) edges edges]
      (if (empty? edges)
        adjlist
        (recur (register-edge (first edges) adjlist) (rest edges))))))

(defn- not-simple? ^Boolean [cycle]
  (let [last-index (last cycle)]
    (loop [cycle (drop 1 (drop-last 1 cycle))]
      (cond
       (empty? cycle) false
       (= (first cycle) last-index) true
       :else (recur (rest cycle))))))

(defn- closed? ^Boolean [cycle]
  (= (first cycle) (last cycle)))

(defn- trivial? ^Boolean [cycle]
  (< (count cycle) 4))

(defn- canonical? ^Boolean [cycle]
  (let [n (count cycle)
        ^long a (nth cycle 1)
        ^long b (nth cycle (- n 2))]
    (< a b)))

;; Generate a list of unique, non-trivial cycles.  In order to avoid
;; duplicates from other local lists, only cycles where all of the
;; indices are smaller than u are allowed.
(defn- cycles-at-u [adjlist hood u limit]
  (let [Dijkstra (memo/fifo (partial theory/Dijkstra adjlist hood))]
    (letfn [(isometric? ^Boolean [cycle]
              (let [n (dec (count cycle))
                    n-1 (dec n)]
                (loop [i 0 j 1]
                  (cond
                   (> i n-1) true
                   (> j n-1) (recur (+ i 1) (+ i 2))
                   :else
                   (let [s (nth cycle i)
                         t (nth cycle j)
                         [s t] [(min s t) (max s t)]
                         dist-cycle (min (- j i) (+ (- i j) n))
                         dist-graph (get (Dijkstra s) t)]
                     (if (< dist-graph dist-cycle)
                       false
                       (recur i (inc j))))))))]
      (loop [stack (vector (vector-of :long u)) finished-cycles (list)]
        (if (empty? stack)
                                        ; the stack is empty, so return the list of finished cycles
          (filter isometric? (filter canonical? (remove trivial? finished-cycles)))
                                        ; otherwise, work on the cycles on top of the stack
          (let [current-cycle (peek stack)
                n (count current-cycle)]
            (cond
                                        ; discard non-simple cycle
             (not-simple? current-cycle) (recur (pop stack) finished-cycles)
                                        ; the cycle is closed, transfer to finished list
             (and (> n 1) (closed? current-cycle))
             (recur (pop stack) (conj finished-cycles current-cycle))
                                        ; cycle too long
             (> n limit) (recur (pop stack) finished-cycles)
                                        ; extend the simple, open path
             :else
             (recur
                                        ; this sexp returns the new stack:
              (let [stack (pop stack)
                    neighbors (remove #(< % u) (filter hood (get adjlist (last current-cycle))))]
                (loop [neighbors neighbors stack stack]
                  (if (empty? neighbors)
                    stack
                    (recur (rest neighbors) (conj stack (conj current-cycle (first neighbors)))))))
              finished-cycles))))))))

;; Return the set of edges which bound the cycle.
(defn- cycle-edges [cycle]
  (loop [cycle cycle edges #{}]
    (if (< (count cycle) 2)
      edges
      (recur (rest cycle) (conj edges #{(first cycle) (second cycle)})))))

;; Remove those cycles which can easily be seen to be inside of the
;; model and/or non-manifold.
(defn- manifold [cycles]
  (let [boundaries (pmap cycle-edges cycles)
        cycles+ (map list cycles boundaries)
        edge-counts (frequencies (mapcat identity boundaries))]
    (loop [inbox cycles+ outbox (list) edge-counts edge-counts]
      (if (empty? inbox)
        outbox
        (let [[cycle boundary] (first inbox)]
          (if (some #(< (get edge-counts %) 3) boundary)
            (recur (rest inbox) (conj outbox cycle) edge-counts)
            (recur (rest inbox) outbox
                   (merge-with - edge-counts (into {} (map #(vector % 1) boundary))))))))))

;; Triangulate a (non-triangular) cycle by connecting the nearest pair
;; that is not already connected.
(defn- triangulate-cycle [points cycle]
  (if (= 4 (count cycle))
                                        ; triangle, report it
    (list cycle)
                                        ; non-triangle, split it
    (let [n (dec (count cycle))
          dist-cycle (fn [i j]
                       (let [i (second i)
                             j (second j)]
                         (min (- j i) (+ (- i j) n))))
          dist-euclidean (fn [u v]
                           (let [u (first u)
                                 v (first v)]
                             (theory/distance points u v)))]
      (let [cycle+ (map list cycle (range))
            uv
                                        ; this sexp returns the best edge at which to split
            (vec (loop [U (rest cycle+) V (rest (rest cycle+))
                        best nil best-dist Double/POSITIVE_INFINITY]
                   (let [u (first U)
                         v (first V)]
                     (cond
                                        ; done
                      (nil? u) best
                                        ; increment u
                      (nil? v) (recur (rest U) (rest (rest U)) best best-dist)
                                        ; see if uv is a better edge
                      (and
                       (> (dist-cycle u v) 1)
                       (< (dist-euclidean u v) best-dist))
                      (recur U (rest V) (list u v) (dist-euclidean u v))
                                        ; edge not better, increment j
                      :else (recur U (rest V) best best-dist)))))
            [i j] (map second uv)
            cycle-1 (apply vector-of :long (concat (take (+ 1 i) cycle) (drop j cycle)))
            cycle-2 (apply vector-of :long (concat (drop i (take (+ 1 j) cycle)) (list (nth cycle i))))]
        (concat
         (triangulate-cycle points cycle-1)
         (triangulate-cycle points cycle-2))))))

(defn- problem-points [index-set surface]
  (let [whacky-edges (map first (filter #(= 1 (second %)) (frequencies (mapcat cycle-edges surface))))
        whacky-points (set (mapcat identity whacky-edges))
        unused-points (set/difference index-set (set (flatten surface)))]
    (set/union whacky-points unused-points)))

(defn- radius-bump [k]
  (let [r (+ (Math/sqrt k) *Δr*)]
    (long (* r r))))

;; Recover a surface from an organized collection of point samples by
;; computing a subset of the Delaunay Triangulation (assuming the
;; sample conditions hold).
(defn compute-surface
  ([points k]
     (compute-surface points k 7))
  ([points k countdown]
     (let [kdtree (kdtree/build points)
           n (count points)
           k (radius-bump k)]
       (loop [surface (list) ; surface to be incrementally built up
              index-set (set (range n)) ; the set of indices of vertices to work on
              epsilon 0.0 ; how much to fudge the relative neighborhood graph
              limit *max-cycle-size* ; the maximum size of any face boundary
              countdown countdown]
         (println "|Γ| ="(count index-set) "\t|γ| =" limit "\tϵ =" epsilon )
         (let [edges (theory/RNG points index-set kdtree (+ 1.0 epsilon) k)
               adjlist (compute-adjlist (count points) edges)]
           (letfn [(compute-cycles [u]
                     (let [u-set (set (map :index (kdtree/query (get points u) kdtree k)))
                           hood (set/intersection u-set index-set)]
                       (cycles-at-u adjlist hood u limit)))]
             (let [patch (mapcat identity (pmap compute-cycles index-set))
                   surface (manifold (concat surface patch))
                   index-set (problem-points index-set surface)]
               (if (and (not (empty? index-set)) (> countdown 0))
                 (recur surface index-set (+ epsilon 0.05) (inc limit) (dec countdown))
                 (mapcat identity (pmap #(triangulate-cycle points %) surface))))))))))
