(ns mst.reconstruct
  (:require [clojure.core.memoize :as memo]
            [mst.kdtree :as kdtree]
            [mst.graph_theory :as theory]))

(def ^:dynamic *max-cycle-size* 7)
(def ^:dynamic *max-hole-size* (* 7 7))
(def ^:dynamic *Δr* 3)

(defn- radius-bump [k]
  (let [r (+ (Math/sqrt k) *Δr*)]
    (long (* r r))))

;; Compute (some of) the edges in the graph that is the reconstructed surface.
(defn compute-edges [points kdtree k]
  (theory/RNG points kdtree k))

;; Register and edge in an adjacency list.
(defn register-edge [edge adjlist]
  (let [[a b] (vec edge)
        a-list (conj (get adjlist a) b)
        b-list (conj (get adjlist b) a)]
    (assoc adjlist a a-list b b-list)))

;; Compute the graph adjacency list.
(defn compute-adjlist [n edges]
  (loop [adjlist (vec (repeat n #{})) edges edges]
    (if (empty? edges)
      adjlist
      (recur (register-edge (first edges) adjlist) (rest edges)))))

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
        ^Long a (nth cycle 1)
        ^Long b (nth cycle (- n 2))]
    (< a b)))

;; Generate a list of unique, non-trivial cycles.  In order to avoid
;; duplicates from other local lists, only cycles where all of the
;; indices are smaller than u are allowed.
(defn cycles-at-u [adjlist hood u limit]
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
      (loop [stack (vector (vector u)) finished-cycles (list)]
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


;; Add cycles composed of entirely of half-edges to patch holes.
(defn holes [cycles limit]
  (let [boundaries (pmap cycle-edges cycles)
        edges (mapcat identity boundaries)
        edge-counts (frequencies edges)
        half-edges (filter #(= 1 (get edge-counts %)) edges) ; all half edges
        half-hood (set (mapcat identity half-edges)) ; neighborhood holding all half edges
        half-n (reduce max (conj half-hood 0)) ; size of the half-edge neighborhood
        half-adjlist (compute-adjlist half-n half-edges)
        half-cycles (for [u half-hood] (cycles-at-u half-adjlist half-hood u limit))]
    (mapcat identity half-cycles)))

;; Remove those cycles which can easily be seen to be inside of the
;; model and/or non-manifold.
(defn manifold [cycles]
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
(defn triangulate-cycle
  ([points cycle]
     (triangulate-cycle points nil nil cycle))
  ([points adjlist kdtree k cycle]
     (let [hood
           (loop [cycle (rest cycle) hood (list)]
             (if (not (empty? cycle))
               (recur (rest cycle)
                      (concat hood (map :index (kdtree/query (nth points (first cycle)) kdtree k))))
               (set hood)))]
       (triangulate-cycle points adjlist hood cycle)))
  ([points adjlist hood cycle]
     (if (= 4 (count cycle))
                                        ; triangle, report it
       (list cycle)
                                        ; non-triangle, split it
       (let [n (dec (count cycle))
             dist-cycle (fn [i j]
                          (let [i (second i)
                                j (second j)]
                            (min (- j i) (+ (- i j) n))))
             dist-graph (if (or (nil? adjlist) (nil? hood))
                                        ; no adjacency list and/or neighborhood: trivial function
                          (fn [_ _] Long/MAX_VALUE)
                                        ; otherwise, do Dijkstra's
                          (let [Dijkstra (memo/fifo (partial theory/Dijkstra adjlist hood))]
                            (fn [s t]
                              (let [s (first s)
                                    t (first t)
                                    [s t] [(min s t) (max s t)]]
                                (get (Dijkstra s) t Long/MAX_VALUE)))))
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
                          (<= (dist-cycle u v) (dist-graph u v))
                          (< (dist-euclidean u v) best-dist))
                         (recur U (rest V) (list u v) (dist-euclidean u v))
                                        ; edge not better, increment j
                         :else (recur U (rest V) best best-dist)))))
               [u v] (map first uv)
               [i j] (map second uv)
               adjlist (register-edge #{u v} adjlist)
               cycle-1 (concat (take (+ 1 i) cycle) (drop j cycle))
               cycle-2 (concat (drop i (take (+ 1 j) cycle)) (list (nth cycle i)))]
           (concat
            (triangulate-cycle points adjlist hood cycle-1)
            (triangulate-cycle points adjlist hood cycle-2)))))))

;; Recover a surface from an organized collection of point samples on
;; its surface by computing a subset of the Delaunay Triangulation
;; (that is, assuming the sample conditions hold).
(defn compute-surface
  ([points k]
     (let [kdtree (kdtree/build points)
           edges (compute-edges points kdtree k)
           adjlist (compute-adjlist (count points) edges)]
       (compute-surface points kdtree edges adjlist k)))
  ([points kdtree edges adjlist k]
     (let [n (count points)
           k (radius-bump k)]
       (letfn [(u-to-fan [u]
                 (let [hood (set (map :index (kdtree/query (get points u) kdtree k)))]
                   (cycles-at-u adjlist hood u *max-cycle-size*)))]
         (let [triangulate-face (partial triangulate-cycle points)
               all-cycles (mapcat identity (pmap u-to-fan (range n)))
               surface-triangles (mapcat triangulate-face (manifold all-cycles))

               tri-edges (mapcat cycle-edges surface-triangles)
               ;; tri-edges (mapcat identity tri-boundaries)
               tri-adjlist (compute-adjlist n tri-edges)
               triangulate-hole (partial triangulate-cycle points tri-adjlist kdtree k)
               hole-triangles (mapcat triangulate-hole (holes surface-triangles *max-hole-size*))
               ;; boundaries (pmap cycle-edges surface-cycles)
               ;; edges (apply concat boundaries)
               ;; edge-counts (frequencies edges)
               ;; hole-cycles (holes surface-cycles edges edge-counts *max-hole-size*) ; holes in the surface

               ;; surface-triangles (apply concat (pmap triangulate-face surface-cycles))
               ;; tri-boundaries (pmap cycle-edges surface-triangles)
               ;; tri-edges (apply concat tri-boundaries)
               ;; tri-adjlist (compute-adjlist n tri-edges)
               ;; triangulate-hole (partial triangulate-cycle points tri-adjlist kdtree k)
               ;; hole-triangles (apply concat (pmap triangulate-hole hole-cycles))
               ]
           (concat surface-triangles hole-triangles)
           ;; surface-triangles
           ;; {:edges edges :adjlist adjlist}
           )))))
