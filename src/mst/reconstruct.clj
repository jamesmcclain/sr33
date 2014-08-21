(ns mst.reconstruct
  (:require [clojure.set :as set]
            [clojure.core.memoize :as memo]
            [mst.kdtree :as kdtree]
            [mst.graph_theory :as theory]))

(def ^:dynamic *Δr* 3)
(def ^:dynamic *max-cycle-size* 7)
(def ^:dynamic *max-hole-size* 21)

(defn- compute-adjlist [^long n edges]
  (letfn [(add-edge [edge adjlist]
            (let [[a b] (vec edge)
                  a-list (conj (get adjlist a) b)
                  b-list (conj (get adjlist b) a)]
              (assoc adjlist a a-list b b-list)))]
    (loop [adjlist (vec (repeat n #{})) edges edges]
      (if (empty? edges)
        adjlist
        (recur (add-edge (first edges) adjlist) (rest edges))))))

(defn- not-simple? ^Boolean [cycle]
  (let [last-index (last cycle)]
    (loop [cycle (drop 1 (drop-last 1 cycle))]
      (cond
       (empty? cycle) false
       (== (first cycle) last-index) true
       :else (recur (rest cycle))))))

(defn- closed? ^Boolean [cycle]
  (== (first cycle) (last cycle)))

(defn- trivial? ^Boolean [cycle]
  (< (count cycle) 4))

(defn- canonical? ^Boolean [cycle]
  (let [^long a (nth cycle 1)
        ^long b (nth cycle (- (count cycle) 2))]
    (< a b)))

;; Generate a list of unique, non-trivial cycles.  In order to avoid
;; duplicates from other local lists, only cycles where all of the
;; indices are smaller than u are allowed.
(defn- cycles-at-u [adjlist hood limit ^long u]
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
                         dist-cycle (min (- j i) (+ (- i j) n))
                         dist-graph (get (Dijkstra (min s t)) (max s t))]
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

(defn- triangle-edges [triangle]
  (let [triangle (seq triangle)
        ^double a (nth triangle 0)
        ^double b (nth triangle 1)
        ^double c (nth triangle 2)]
    (list #{a b} #{b c} #{c a})))

;; Remove those cycles which can easily be seen to be inside of the
;; model and/or non-manifold.
(defn- manifold [cycles]
  (let [boundaries (pmap cycle-edges cycles)
        cycles+ (map list cycles boundaries)
        edge-counts (frequencies (apply concat boundaries))
        inner-cycle? (fn [[cycle boundary]] (every? #(> (get edge-counts %) 2) boundary))]
    (map first (remove inner-cycle? cycles+))))

;; Triangulate a (non-triangular) cycle by connecting the nearest pair
;; that is not already connected.
(defn- triangulate-cycle [points cycle]
  (if (== 4 (count cycle))
                                        ; triangle, report it
    (let [^long a (nth cycle 0)
          ^long b (nth cycle 1)
          ^long c (nth cycle 2)]
      (hash-set (hash-set a b c)))
                                        ; non-triangle, split it
    (let [n (dec (count cycle))
          dist-cycle (fn [i j]
                       (let [^long i (second i)
                             ^long j (second j)]
                         (min (- j i) (+ (- i j) n))))
          dist-euclidean (fn [u v]
                           (let [^long u (first u)
                                 ^long v (first v)]
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
            ;; [i j] (map second uv)
            ^long i (second (first uv))
            ^long j (second (second uv))
            cycle-1 (apply vector-of :long (concat (take (+ 1 i) cycle) (drop j cycle)))
            cycle-2 (apply vector-of :long (concat (drop i (take (+ 1 j) cycle)) (list (nth cycle i))))]
        (set/union
         (triangulate-cycle points cycle-1)
         (triangulate-cycle points cycle-2))))))

;; Return a set of points in whose vicinity the surface should be
;; re-reconstructed.
(defn- problem-points [index-set surface]
  (let [whacky-edges (map first (filter #(== 1 (second %)) (frequencies (mapcat cycle-edges surface))))
        whacky-points (set (apply concat whacky-edges))
        unused-points (set/difference index-set (set (flatten surface)))]
    (set/union whacky-points unused-points)))

;; bump the area of a neighborhood.
(defn- bump-radius ^long [k]
  (let [r (+ (Math/sqrt k) *Δr*)]
    (long (* r r))))

;; Recover a surface from an organized collection of point samples by
;; computing a subset of the Delaunay Triangulation (assuming the
;; sample conditions hold) then triangulating the resulting cycles.
(defn compute-surface [points k tries]
  (let [kdtree (kdtree/build points)
        n (count points)
        r (bump-radius k)
        k-hood-of (fn [u] (set (map :index (kdtree/query (nth points u) kdtree k))))
        r-hood-of (fn [u] (set (map :index (kdtree/query (nth points u) kdtree r))))
        fudge (Math/pow (Math/sqrt 2.001) (/ 1 tries))]
    (loop [old-surface (list)
           old-graph (list)
           index-set (set (range n))
           epsilon 1.0
           limit *max-cycle-size*
           countdown tries]
      (println (java.util.Date.) "\t|Γ| ="(count index-set) "\tϵ =" epsilon)
      (let [index-hood 
            (if (== n (count index-set))
              index-set
              (set (apply concat (pmap r-hood-of index-set))))
            graph (concat (theory/RNG points index-hood k-hood-of epsilon) old-graph)
            adjlist (compute-adjlist n graph)
            get-cycles (fn [u] (cycles-at-u adjlist (r-hood-of u) (long limit) u))
            salvageable (remove #(index-hood (first %)) old-surface) ; salvage as much as possible
            patch (apply concat (pmap get-cycles index-hood)) ; (re)compute the non-salvageable part
            surface (manifold (concat salvageable patch)) ; extract a new surface
            new-index-set (problem-points index-set surface)]
        (if (and (not (empty? new-index-set)) (> countdown 1))
          (recur surface graph new-index-set (* epsilon fudge) (+ limit (/ 3 tries)) (dec countdown))
          (do
            (println (java.util.Date.) "△")
            (let [surface (apply set/union (pmap #(triangulate-cycle points %) surface))]
              (println (java.util.Date.) "○")
              (let [edge-counts (frequencies (apply concat (pmap triangle-edges surface)))
                    half-edges (map first (filter #(== 1 (second %)) edge-counts))
                    half-adjlist (compute-adjlist n half-edges)
                    half-index-set (set (flatten (map seq half-edges)))
                    get-holes (fn [u] (cycles-at-u half-adjlist half-index-set *max-hole-size* u))
                    holes (apply concat (pmap get-holes half-index-set))]
                (println (java.util.Date.) "holes found: " (count holes))
                (set/union surface (apply set/union (pmap #(triangulate-cycle points %) holes)))))))))))
