(ns mst.graph_theory
  (:require [quil.core :as q]
            [clojure.set :as set]
            [mst.kdtree :as kdtree])
  (:use [clojure.data.priority-map]))

;; --------------------------------------------------

;; Local nearest-neighbors.
(defn local-nn [hood jitter]
  (let [index (:index (first hood))
        hood (drop 1 hood)
        radius (* jitter (:dist (first hood)))
        hood (map :index (filter #(< (:dist %) radius) hood))]
    (map #(hash-set index %) hood)))

;; The Nearest Neighbor Graph.
(defn NN [points kdtree k jitter]
  (letfn [(point->hood [xyz] (kdtree/query xyz kdtree k))
          (hood->edges [hood] (local-nn hood jitter))]
    (apply concat (pmap (comp hood->edges point->hood) points))))

;; --------------------------------------------------

;; A local version of Prim's Algorithm.
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
              mst
              (recur (conj covered u v) (dissoc Q #{u v}) (conj mst #{u v})))))))))

;; Give the Minimum Spanning Tree.
(defn MST [points kdtree k]
  (letfn [(point->edges [xyz] (local-prim (kdtree/query xyz kdtree k)))]
    (apply concat (pmap point->edges points))))

;; --------------------------------------------------

;; CLRS page 541.
(defn DFSVisit [u Adj [color pi non-tree]]
  (let [[color pi non-tree]
        (loop [Adju (seq (Adj u))
               color (assoc color u :grey)
               pi pi
               non-tree non-tree]
          (if (not (empty? Adju))
            ;; v in Adj[u]
            (let [v (first Adju)]
              (cond
               ;; v has not been seen, explore
               (nil? (get color v))
               (let [[color pi non-tree] (DFSVisit v Adj [color (assoc pi v u) non-tree])]
                 (recur (rest Adju) color pi non-tree))
               ;; v has been seen, back-edge
               (= :grey (get color v)) (recur (rest Adju) color pi (conj non-tree (list u v)))
               ;; v has been seen, forward- or cross-edge
               (= :black (get color v)) (recur (rest Adju) color pi non-tree)))
            ;; done looking at Adj[u]
            [color pi non-tree]))
        color (assoc color u :black)]
    [color pi non-tree]))

;; CLRS page 541.
(defn DFS [u adjlist hood]
  (letfn [(Adj [v] (set/intersection (get adjlist v) hood))]
    (let [color (hash-map)
          pi (hash-map u nil)
          non-tree (list)
          [color pi non-tree] (DFSVisit u Adj [color pi non-tree])]
      [pi non-tree])))

;; --------------------------------------------------

;; Relative Neighborhood Graph.
(defn RNG [points kdtree k]
  (letfn [(sq ^Double [^Double x] (* x x))
          (distance ^Double [^Long u ^Long v]
            (let [abc (nth points u)
                  ^Double a (nth abc 0)
                  ^Double b (nth abc 1)
                  ^Double c (nth abc 2)
                  xyz (nth points v)
                  ^Double x (nth xyz 0)
                  ^Double y (nth xyz 1)
                  ^Double z (nth xyz 2)]
              (+ (sq (- a x)) (sq (- b y)) (sq (- c z)))))

          ;; check to see if (p,q) is an RNG edge
          (check-edge [p q d hood]
            ;; For all r, see if d(r,p) < d(p,q) and d(r,q) < d(p,q).
            ;; If either is true, then the edge (p,q) is not a member
            ;; of the RNG.
            (loop [hood (disj hood p q)]
              (if (not (empty? hood))
                ;; not done checking, so check
                (let [r (first hood)]
                  (if (and (< (distance p r) d)
                           (< (distance q r) d))
                    ;; failed this test
                    nil
                    ;; passed this one test
                    (recur (rest hood))))
                ;; done checking, passed all of the tests
                #{p q})))

          ;; report all RNG edges (u,v) where u < v for some given u
          (at-u [u]
            (let [hood (set (map :index (kdtree/query (nth points u) kdtree k)))
                  candidates (set (filter #(< u %) hood))]
              ;; for every v near u where u < v
              (loop [candidates candidates edges (list)]
                (if (not (empty? candidates))
                  ;; check to see if (u,v) is in the RNG
                  (let [v (first candidates)
                        d (distance u v)]
                    (recur (disj candidates v) (conj edges (check-edge u v d (disj hood u v)))))
                  ;; return the found edges
                  (remove nil? edges)))))]
    ;; report all edges in the RNG
    (set (apply concat (pmap at-u (range (count points)))))))

;; Nearest Neighbor Graph + Minimum Spanning Tree.
(defn NN+MST [points kdtree k jitter]
  (let [nn (NN points kdtree k jitter)
        mst (MST points kdtree k)]
    (set (concat nn mst))))
