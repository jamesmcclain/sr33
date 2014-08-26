(ns ^{:author "James McClain <jwm@daystrom-data-concepts.com>"}
  mst.reconstruct
  (:require [clojure.set :as set]
            [clojure.core.memoize :as memo]
            [clojure.core.reducers :as r]
            [mst.kdtree :as kdtree]
            [mst.graph_theory :as theory]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; Compute an adjacency list (a hash map) from a collection of edges.
(defn compute-adjlist [edges]
  (letfn [(staple
            ([] {})
            ([a b] (merge-with set/union a b)))
          (add-edge [edge]
            (let [a (first edge)
                  b (second edge)]
              {a #{b} b #{a}}))]
    (r/fold staple (r/map add-edge edges))))

;; Generate a list of unique, non-trivial cycles that meet the point
;; with index u.  In order to avoid duplicates from other local lists,
;; only cycles where all of the indices are smaller than u are
;; allowed.
(defn- cycles-at-u [adjlist hood u]
  (let [compute-dist (memo/fifo (fn [s] (first (theory/Dijkstra adjlist hood s))))
        eligible (set (filter #(< % u) hood))
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
      (let [neighbors (set/intersection eligible (get adjlist u #{}))
            candidate-cycles
            (for [a neighbors b neighbors :when (< a b)] ; enforcing a < b ensures only one copy of each cycle
              (let [pi (compute-pi a)
                    path (trace-path a b pi)]
                (if (not (nil? path))
                  (concat (list u) path (list u)))))]
        (filter isometric? (remove nil? candidate-cycles))))))

;; Return the set of edges which bound the cycle.
(defn- compute-boundary [cycle]
  (loop [cycle cycle edges (list)]
    (if (< (count cycle) 2)
      edges
      (recur (rest cycle) (conj edges #{(first cycle) (second cycle)})))))

;; Triangulate a (non-triangular) cycle by connecting the nearest pair
;; that is not already connected.
(defn- triangulate-cycle [points cycle]
  (if (== 4 (count cycle))
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
                      (nil? u) best ; done
                      (nil? v) (recur (rest U) (rest (rest U)) best best-dist) ; increment
                      (and (> (dist-cycle u v) 1)
                           (< (dist-euclidean u v) best-dist))
                      (recur U (rest V) (list u v) (double (dist-euclidean u v))) ; see if uv is a better edge
                      :else (recur U (rest V) best best-dist)))))
            [[_ i] [_ j]] uv
            cycle-1 (concat (take (+ 1 i) cycle) (drop j cycle))
            cycle-2 (concat (drop i (take (+ 1 j) cycle)) (list (nth cycle i)))]
        (set/union
         (triangulate-cycle points cycle-1)
         (triangulate-cycle points cycle-2))))))

;; Find holes in the surface.
(defn find-holes [surface]
  (let [edge-counts (frequencies (r/mapcat :boundary surface))
        half-edges (r/map first (r/filter #(== (val %) 1) edge-counts)) ; half edges
        adjlist (compute-adjlist (into '() half-edges))
        half-edge-verts (r/reduce set/union half-edges) ; XXX why won't fold?
        compute-holes (fn [u] (cycles-at-u adjlist half-edge-verts u))
        holes (r/mapcat compute-holes half-edge-verts)]
    [holes edge-counts half-edges]))

;; Look for half-edges that are not part of a hole and edges that meet
;; too many faces.  any faces meeting such edges are deemed to be
;; suspicious.
(defn remove-suspicious-faces [surface]
  (let [[holes edge-counts half-edges] (find-holes surface)
        hole-edges (into #{} (r/mapcat compute-boundary holes)) ; half edges around holes
        bad-edges (into #{} (r/remove hole-edges half-edges)) ; half edges not around any hole
        overloaded-edges (into #{} (r/map first (r/filter #(> (val %) 2) edge-counts)))
        suspicious-face? (fn [complex]
                           (let [boundary (get complex :boundary)]
                             (or (some overloaded-edges boundary)
                                 (some bad-edges boundary))))]
    (r/remove suspicious-face? surface)))

;; Remove faces that are (likely to be) inside of the model (assuming
;; that it is closed).
(defn remove-inner-faces [surface]
  (let [edge-counts (frequencies (r/mapcat :boundary surface))
        inner-face? (fn [complex]
                      (let [boundary (get complex :boundary)
                            sum (double (reduce + (map edge-counts boundary)))
                            avg-double (- (/ sum (count boundary)) 0.0001)
                            avg-int (Math/round avg-double)]
                        ;; (if (> avg-int 2)
                        ;;   (println avg-double (get complex :cycle) (map edge-counts boundary)))
                        (> avg-int 2)))]
    (r/remove inner-face? surface)))

;; Recover a surface from an organized collection of point samples by
;; computing a subset of the Delaunay Triangulation (assuming the
;; sample conditions hold) then triangulating the resulting cycles.
(defn compute-surface [points k hole-limit]
  (let [kdtree (kdtree/build points)
        n (count points)
        k-hood-of (memo/fifo
                   (fn [u] (set (map :index (kdtree/query (nth points u) kdtree k))))
                   :fifo/threshold (inc n))
        triangulate-complex (fn [complex]
                              (let [ts (triangulate-cycle points (get complex :cycle))
                                    bs (map compute-boundary ts)]
                                (map #(hash-map :cycle %1 :boundary %2) ts bs)))
        face-to-set (fn [face]
                      (set (take 3 face)))]
    (let [graph (theory/RNG points (set (range n)) k-hood-of) ; relative neighborhood graph
          adjlist (compute-adjlist graph) ; adjacency list of the same
          compute-cycles (fn [u] (cycles-at-u adjlist (k-hood-of u) u))
          surface (apply concat (pmap compute-cycles (range n))) ; isometric cycles
          surface (r/map #(hash-map :cycle % :boundary (compute-boundary %)) surface) ; cycles + boundaries
          surface (remove-inner-faces surface) ; remove inner faces
          surface (r/mapcat triangulate-complex surface) ; triangulate
          surface (remove-suspicious-faces surface) ; remove faces that meet suspicious edges
          [holes _ _] (find-holes surface) ; find remaining holes          
          holes (r/filter #(<= (count %) hole-limit) holes)
          
          surface (into #{} (r/map (comp face-to-set :cycle) surface))
          holes (into #{} (r/map face-to-set (r/mapcat #(triangulate-cycle points %) holes)))]
      (set/union holes surface))))
