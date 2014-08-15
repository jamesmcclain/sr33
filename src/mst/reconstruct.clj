(ns mst.reconstruct
  (:require [clojure.core.memoize :as memo]
            [mst.kdtree :as kdtree]
            [mst.graph_theory :as theory]))

(def ^:dynamic *max-cycle-size* 7)

;; Compute (some of) the edges in the graph that is the reconstructed surface.
(defn compute-edges [points kdtree k]
  (theory/RNG points kdtree k))

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

(defn not-simple? ^Boolean [cycle]
  (let [last-index (last cycle)]
    (loop [cycle (drop 1 (drop-last 1 cycle))]
      (cond
       (empty? cycle) false
       (= (first cycle) last-index) true
       :else (recur (rest cycle))))))

(defn closed? ^Boolean [cycle]
  (= (first cycle) (last cycle)))

(defn trivial? ^Boolean [cycle]
  (< (count cycle) 4))

(defn canonical? ^Boolean [cycle]
  (let [n (count cycle)
        ^Long a (nth cycle 1)
        ^Long b (nth cycle (- n 2))]
    (< a b)))

;; Generate a list of unique, non-trivial loops.  In order to avoid
;; duplicates from other local lists, only loops where all of the
;; indices are smaller than u are allowed.
(defn loops-at-u [adjlist hood u limit]
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
      (loop [stack (vector (vector u)) finished-loops (list)]
        (if (empty? stack)
          ;; The stack is empty, so return the list of finished loops.
          (filter isometric? (filter canonical? (remove trivial? finished-loops)))
          ;; Work on the loop on top of the stack.
          (let [current-loop (peek stack)]
            (cond
                                        ; discard non-simple loop
             (not-simple? current-loop) (recur (pop stack) finished-loops)
                                        ; triangle is closed, transfer to finished list
             (and (> (count current-loop) 1) (closed? current-loop))
             (recur (pop stack) (conj finished-loops current-loop))
                                        ; loop too long
             (> (count current-loop) limit) (recur (pop stack) finished-loops)
                                        ; extend the simple, open loop
             :else
             (recur
                                        ; this sexp returns the new stack:
              (let [stack (pop stack)
                    neighbors (remove #(< % u) (filter hood (get adjlist (last current-loop))))]
                (loop [neighbors neighbors stack stack]
                  (if (empty? neighbors)
                    stack
                    (recur (rest neighbors) (conj stack (conj current-loop (first neighbors)))))))
              finished-loops))))))))

;; Return the set of edges which bound the cycle.
(defn cycle-edges [cycle]
  (loop [cycle cycle edges #{}]
    (if (< (count cycle) 2)
      edges
      (recur (rest cycle) (conj edges #{(first cycle) (second cycle)})))))


;; Add cycles composed of entirely of half-edges to patch holes.
(declare loops-at-u)
(defn holes [cycles]
  (let [boundaries (map cycle-edges cycles) ; edges around each cycle
        edges (apply concat boundaries) ; all edges
        freqs (frequencies edges) ; the number of times each edge occurs
        half-edges (filter #(= 1 (get freqs %)) edges) ; all half edges
        half-hood (set (flatten (map seq half-edges))) ; neighborhood holding all half edges
        half-n (reduce max (conj half-hood 0)) ; size of the half-edge neighborhood
        half-adjlist (compute-adjlist half-n half-edges)]
    (apply concat (remove empty? (for [u half-hood] (loops-at-u half-adjlist half-hood u Long/MAX_VALUE))))))

;; Remove those cycles which can easily be seen to be inside of the
;; model and/or non-manifold.
(defn manifold [cycles]
  (let [boundaries (map cycle-edges cycles) ; edges around each cycle
        edges (apply concat boundaries) ; all edges
        freqs (frequencies edges) ; the number of times each edge occurs
        good? (fn [boundary] (some #(= % 2) (map #(get freqs %) boundary))) ; at least one good edge?
        inside (map good? boundaries)]
    (pmap first (filter #(second %) (map list cycles inside)))))

;; Triangulate a (non-triangular) cycle by connecting the nearest pair
;; that is not already connected.
(defn triangulate-cycle
  ([points cycle]
     (let [outer (cycle-edges cycle)
           inner ; inner edges + lengths
           (loop [I (rest cycle) J (rest (rest cycle)) inner '()]

             (let [i (first I)
                   j (first J)]
               (cond
                (nil? i) (sort-by second inner)
                (nil? j) (recur (rest I) (rest (rest I)) inner)
                (outer #{i j}) (recur I (rest J) inner)
                :else (recur I (rest J) (conj inner (list #{i j} (theory/distance points i j)))))))]
       (triangulate-cycle points cycle outer inner)))
  ([points cycle outer inner]
     (if (= 4 (count cycle))
                                        ; triangle, done
       (list cycle)
                                        ; non-triangle, split
       (let [cycle-set (set cycle)
             in-cycle? (fn [[edge _]] (and (not (outer edge)) (every? cycle-set edge)))
             inner (filter in-cycle? inner)
             edge (first (first inner))
             outer (conj outer edge)
             [i j] (map second (filter #(edge (first %)) (map list cycle (range))))
             cycle-1 (concat (take (+ 1 i) cycle) (drop j cycle))
             cycle-2 (concat (drop i (take (+ 1 j) cycle)) (list (nth cycle i)))]
         (concat
          (triangulate-cycle points cycle-1 outer inner)
          (triangulate-cycle points cycle-2 outer inner))))))

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
     (let [k (+ (long (Math/sqrt k)) 3)
           k (* k k)]
       (letfn [(u-to-fan [u]
                 (let [adju (get adjlist u)
                       hood (set (map :index (kdtree/query (get points u) kdtree k)))]
                   (loops-at-u adjlist hood u *max-cycle-size*)))]
         (let [cycles (apply concat (pmap u-to-fan (range (count points))))
               surface-cycles (manifold cycles)
               hole-cycles (holes surface-cycles)]
           (concat hole-cycles surface-cycles))))))
