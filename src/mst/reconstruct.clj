(ns mst.reconstruct
  (:require [clojure.set :as set]
            [clojure.core.memoize :as memo]
            [mst.kdtree :as kdtree]
            [mst.graph_theory :as theory]))

(def ^:dynamic *Δr* 3)

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn- compute-adjlist [n edges]
  (letfn [(add-edge [edge adjlist]
            (let [[a b] (vec edge)
                  a-list (conj (get adjlist a) b)
                  b-list (conj (get adjlist b) a)]
              (assoc adjlist a a-list b b-list)))]
    (loop [adjlist (vec (repeat n #{})) edges edges]
      (if (empty? edges)
        adjlist
        (recur (add-edge (first edges) adjlist) (rest edges))))))

;; Generate a list of unique, non-trivial cycles that meet the point
;; with index u.  In order to avoid duplicates from other local lists,
;; only cycles where all of the indices are smaller than u are
;; allowed.
(defn- cycles-at-u [adjlist hood u]
  (let [compute-dist (memo/fifo (fn [s] (first (theory/Dijkstra adjlist hood s))))
        eligible (set (filter #(< % u) hood)) ; strange asymmetry in performance between < and >
        compute-pi (memo/fifo (fn [s] (second (theory/Dijkstra adjlist eligible s))))]
    (letfn [(trace-path [a b pi]
              (loop [current b path (list)]
                (cond
                 (== current a) (conj path current)
                 (== current -1) nil
                 :else (recur (get pi current -1) (conj path current)))))
            (isometric? ^Boolean [cycle]
              (let [n (dec (count cycle))
                    n-1 (dec n)]
                (loop [i 0 j 1]
                  (cond
                   (> i n-1) true ; no shortcuts found
                   (> j n-1) (recur (+ i 1) (+ i 2)) ; next iteration of i
                   :else ; check for shortcut
                   (let [s (nth cycle i)
                         t (nth cycle j)
                         dist-cycle (min (- j i) (+ (- i j) n))
                         dist-graph (get (compute-dist (min s t)) (max s t))]
                     (if (< dist-graph dist-cycle)
                       false
                       (recur i (inc j))))))))]
      (let [neighbors (set/intersection eligible (get adjlist u))
            candidate-cycles
            (for [a neighbors b neighbors :when (< a b)] ; enforcing a < b ensures only one copy of each cycle
              (let [pi (compute-pi a)
                    path (trace-path a b pi)]
                (if (not (nil? path))
                  (concat (list u) path (list u)))))]
        (filter isometric? (remove nil? candidate-cycles))))))

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
                      (recur U (rest V) (list u v) (double (dist-euclidean u v)))
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
(defn compute-surface [points k tries hole-limit]
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
           countdown tries]
      (println (java.util.Date.) "\t|Γ| ="(count index-set) "\tϵ =" epsilon)
      (let [index-hood 
            (if (== n (count index-set))
              index-set
              (set (apply concat (pmap r-hood-of index-set))))
            graph (concat (theory/RNG points index-hood k-hood-of epsilon) old-graph)
            adjlist (compute-adjlist n graph)
            compute-cycles (fn [u] (cycles-at-u adjlist (r-hood-of u) u))
            salvageable (remove #(index-hood (first %)) old-surface) ; salvage as much as possible
            patch (apply concat (pmap compute-cycles index-hood)) ; (re)compute the non-salvageable part
            surface (manifold (concat salvageable patch)) ; extract a new surface
            new-index-set (problem-points index-set surface)]
        (if (and (not (empty? new-index-set)) (> countdown 1))
          (recur surface graph new-index-set (* epsilon fudge) (dec countdown))
          (do
            (println (java.util.Date.) "△")
            (let [surface (apply set/union (pmap #(triangulate-cycle points %) surface))]
              (if (> hole-limit 0)
                (do
                  (println (java.util.Date.) "○")
                  (let [edge-counts (frequencies (apply concat (pmap triangle-edges surface)))
                        half-edges (map first (filter #(== 1 (second %)) edge-counts))
                        half-adjlist (compute-adjlist n half-edges)
                        half-index-set (set (flatten (map seq half-edges)))
                        get-holes (fn [u] (cycles-at-u half-adjlist half-index-set u))
                        holes (filter #(<= (count %) hole-limit) (apply concat (pmap get-holes half-index-set)))]
                    (println (java.util.Date.) "holes found: " (count holes))
                    (set/union surface (apply set/union (pmap #(triangulate-cycle points %) holes)))))
                surface))))))))
