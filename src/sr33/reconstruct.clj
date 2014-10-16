(ns ^{:author "James McClain <jwm@daystrom-data-concepts.com>"}
  sr33.reconstruct
  (:require [clojure.set :as set]
            [clojure.core.memoize :as memo]
            [clojure.core.reducers :as r]
            [sr33.kdtree :as kdtree]
            [sr33.graph_theory :as theory]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; Compute an adjacency list (a hash map) from a collection of edges.
(defn- compute-adjlist [edges]
  (letfn [(staple
            ([] {})
            ([a b] (merge-with set/union a b)))
          (edge-to-map [uv]
            (let [u (first uv)
                  v (second uv)]
              {u #{v} v #{u}}))
          (add-edge [adjlist edge]
            (merge-with set/union adjlist (edge-to-map edge)))]
    (r/fold staple add-edge edges)))

;; Generate a list of unique, non-trivial cycles that meet the point
;; with index u.  In order to avoid duplicates from other local lists,
;; only cycles where all of the indices are smaller than u are
;; allowed.
;;
;; The ``graph'' parameter is the adjacency list in which cycles are
;; searched-for.  The ``metric'' parameter is the adjacency list of
;; the graph in which distances are defined for purposes of finding
;; isometric cycles.  These two might seemingly always be the same,
;; but they need to be distinct when looking for non-convex holes.
(defn- cycles-at-u [graph metric hood u & extra]
  (let [hood (or hood (set (keys metric)))
        extra (set extra)
        compute-dist (memo/fifo (fn [s] (first (theory/Dijkstra metric hood s))))
        eligible (set (filter #(< (long %) (long u)) hood))
        compute-pi (memo/fifo (fn [s] (second (theory/Dijkstra graph eligible s))))]
    (letfn [(trace-path [a b pi] ; trace a path from a to be using the previous set pi
              (loop [current b path (list)]
                (cond
                 (== (long current) (long a)) (conj path current)
                 (== (long current) -1) nil
                 :else (recur (get pi current -1) (conj path current)))))
            (isometric? [cycle] ; determine if cycle is isometric
              (let [n (dec (count cycle))
                    n-1 (dec n)]
                (loop [i 0 j 1]
                  (cond
                   (> i n-1) true ; no shortcuts found
                   (> j n-1) (recur (+ i 1) (+ i 2)) ; next iteration of i
                   :else ; check for shortcut
                   (let [s (long (nth cycle i))
                         t (long (nth cycle j))
                         dist-cycle (long (min (- j i) (+ (- i j) n)))
                         dist-metric (long (get (compute-dist (min s t)) (max s t)))]
                     (if (< dist-metric dist-cycle)
                       false ; not isometric
                       (recur i (inc j))))))))]
      (let [neighbors (set/intersection eligible (get graph u #{}))
            candidate-cycles
            (for [a neighbors b neighbors :when (< (long a) (long b))] ; enforcing a < b ensures only one copy of each cycle
              (let [pi (compute-pi a) path (trace-path a b pi)]
                (if (not (nil? path))
                  (concat (list u) path (list u)) nil)))
            candidate-cycles (remove nil? candidate-cycles)]
        (if (contains? extra :no-isometry-test)
          candidate-cycles
          (filter isometric? candidate-cycles))))))

;; Return the set of edges which bound the cycle.
(defn- compute-boundary [cycle]
  (loop [cycle cycle edges (list)]
    (if (< (count cycle) 2)
      edges
      (recur (rest cycle) (conj edges #{(first cycle) (second cycle)})))))

;; Turn a cycle into a cycle+boundary pair.
(defn- cycle-to-complex [cycle]
  (hash-map :cycle cycle :boundary (compute-boundary cycle)))

;; Triangulate a (non-triangular) cycle by connecting the nearest pair
;; that is not already connected.
(defn- triangulate-cycle [points cycle]
  (if (== 4 (count cycle)) ; triangle: report it, otherwise: split it
    (list cycle)
    (let [n (dec (count cycle))
          dist-cycle (fn [i j]
                       (let [i (long (second i))
                             j (long (second j))]
                         (min (- j i) (+ (- i j) n))))
          dist-euclidean (fn [u v]
                           (let [u (first u)
                                 v (first v)]
                             (theory/distance points u v)))]
      (let [cycle+ (map list cycle (range))
            uv ; the following sexp returns the best edge at which to split
            (vec (loop [U (rest cycle+) V (rest (rest cycle+))
                        best nil best-dist Double/POSITIVE_INFINITY]
                   (let [u (first U)
                         v (first V)]
                     (cond
                      (nil? u) best ; done
                      (nil? v) (recur (rest U) (rest (rest U)) best best-dist) ; increment
                      (and (> (long (dist-cycle u v)) 1) (< (double (dist-euclidean u v)) best-dist))
                      (recur U (rest V) (list u v) (double (dist-euclidean u v))) ; uv is a better edge
                      :else (recur U (rest V) best best-dist)))))
            [[_ i] [_ j]] uv
            cycle-1 (concat (take (+ 1 (long i)) cycle) (drop j cycle))
            cycle-2 (concat (drop i (take (+ 1 (long j)) cycle)) (list (nth cycle i)))]
        (into
         (triangulate-cycle points cycle-1)
         (triangulate-cycle points cycle-2))))))

;; Find holes in the surface.
(defn- find-holes [surface neighborhood-of & extra]
  (let [edge-counts (frequencies (r/mapcat :boundary surface))
        half-graph-E (r/map first (filter #(== 1 (long (val %))) edge-counts)) ; half-edges
        half-graph (compute-adjlist half-graph-E) ; half-edge graph
        half-graph-V (into [] (r/foldcat (r/flatten (r/map seq half-graph-E)))) ; half-edge graph vertices
        extra (apply hash-map extra)
        metric (get extra :metric false)
        isometry (if (not metric) :no-isometry-test nil)
        compute-holes (fn [u] (cycles-at-u half-graph (or metric half-graph) (neighborhood-of u) u isometry))]
    (r/foldcat (r/map cycle-to-complex (r/mapcat compute-holes half-graph-V)))))

;; Remove faces that are (likely to be) inside of the model (assuming
;; that it is closed).
(defn- remove-inner-faces [surface]
  (let [edge-counts (frequencies (r/mapcat :boundary surface))]
    (letfn [(inner? [complex]
              (let [boundary (get complex :boundary)
                    sum (double (reduce + (map edge-counts boundary)))
                    avg-double (- (/ sum (count boundary)) 0.0001)
                    avg-int (Math/round avg-double)]
                (> avg-int 2)))]
      (r/foldcat (r/remove inner? surface)))))

(defmacro remove-x-triangles [surface predicate quantifier]
  `(let [edge-counts# (frequencies (r/mapcat :boundary ~surface))
         edges# (into #{} (r/foldcat (r/map first (filter ~predicate edge-counts#))))
         x?# (fn [complex#] (~quantifier edges# (get complex# :boundary)))]
     (r/foldcat (r/remove x?# ~surface))))

(defn- remove-suspicious-triangles [surface]
  (remove-x-triangles surface #(> (long (val %)) 2) #'some))

;; Recover a surface from an organized collection of point samples by
;; computing a subset of the Delaunay Triangulation (assuming the
;; sample conditions hold) then triangulating the resulting cycles.
(defn compute-surface [points k]
  (let [kdtree (kdtree/build points)
        n (count points)
        k-hood-of (memo/fifo
                   (fn [u] (set (map :index (kdtree/query (nth points u) kdtree k))))
                   :fifo/threshold (inc n))
        triangulate-surface (fn [surface]
                              (r/foldcat (r/map cycle-to-complex
                                                (r/mapcat #(triangulate-cycle points %)
                                                          (r/map :cycle surface)))))
        tri-to-set (fn [face] (set (take 3 face)))]
    (let [_ (println (java.util.Date.) "\t edges")
          edges (theory/RNG points n k-hood-of) ; relative neighborhood graph

          _ (println (java.util.Date.) "\t isometric cycles")
          graph (compute-adjlist edges) ; adjacency list of the same
          compute-cycles (fn [u] (cycles-at-u graph graph (k-hood-of u) u))
          surface (r/foldcat (r/mapcat compute-cycles (into [] (range n)))) ; isometric cycles
          surface (r/map cycle-to-complex surface) ; cycles + boundaries
          surface (remove-inner-faces surface) ; remove inner faces

          _ (println (java.util.Date.) "\t triangulation")
          surface (triangulate-surface surface) ; triangulate
          graph (compute-adjlist (r/foldcat (r/mapcat :boundary surface)))
          surface (remove-suspicious-triangles surface) ; remove bad triangles

          _ (println (java.util.Date.) "\t convex holes")
          graph (compute-adjlist (r/foldcat (r/mapcat :boundary surface)))
          convex-holes (find-holes surface k-hood-of :metric graph)
          surface (r/cat surface (triangulate-surface convex-holes)) ; add convex holes

          _ (println (java.util.Date.) "\t non-convex holes")
          graph (compute-adjlist (r/foldcat (r/mapcat :boundary surface)))
          holes (find-holes surface k-hood-of)
          surface (r/cat surface (triangulate-surface holes))]

      (println (java.util.Date.) "\t done")
      (into #{} (r/foldcat (r/map (comp tri-to-set :cycle) surface))))))
