(ns mst.graph_theory
  (:require [quil.core :as q]
            [clojure.set :as set]
            [mst.kdtree :as kdtree])
  (:use [clojure.data.priority-map]))

(defn nn-1 [points tree k jitter]
  (loop [points+ (map list points (range)) graph (hash-set)]
    (if (empty? points+)
      ;; If done, return the nearest neighbor graph.
      (map vec graph)
      ;; Otherwise (continue) build(ing) the graph.
      (let [xyz (first (first points+))
            i (second (first points+))
            [x y z] xyz
            near (remove #(= (get % :index) i) (kdtree/query xyz tree k))
            small (* jitter (get (first near ) :dist))
            near (map :index (filter #(<= (get % :dist) small) near))
            edges (map #(hash-set i %) near)]
        (recur (rest points+) (apply conj graph edges))))))

(defn prim [points tree k]
  (letfn [(neighbors [xyz i]
            (map #(hash-set i %) (filter #(< i %) (map :index (kdtree/query xyz tree k)))))
          (dist [xyz abc]
            (apply q/dist (concat xyz abc)))
          (edist [uv]
            (let [[u v] (vec uv)]
              (dist (nth points u) (nth points v))))]
    (let [edges (apply concat (map neighbors points (range)))
          edges (map first (sort-by second (map list edges (map edist edges))))
          [u v] (vec (first edges))]
      ;; The edge (u,v) is the first edge in the length-sorted list.
      ;; Add that to the minimum spanning tree (3) so u and v are not
      ;; uncovered (1).  Remove the first edge from the list of edges
      ;; that are under consideration (2).
      (loop [uncovered (disj (into (hash-set) (range (count points))) u v) ; 1
             edges (rest edges) ; 2
             mst (conj (list) (first edges))] ; 3
        (let [edges- (filter #(= 1 (count (set/intersection uncovered %))) edges)]
          (if (or (empty? uncovered) (empty? edges-))
                                        ; if done, return
            (map vec mst)
                                        ; otherwise, add an edge
            (let [edge (first edges-)
                  [u v] (vec edge)]
              ;; The endpoints u and v are now covered (1).  If both
              ;; endpoints of an edge are already covered, it is not
              ;; needed any longer (2).  Add the edge (u,v) to the
              ;; minimum spanning tree (3).
              (let [uncovered (disj uncovered u v) ; 1
                    edges (remove #(empty? (set/intersection uncovered %)) edges) ; 2
                    mst (conj mst edge)] ; 3
                (recur uncovered edges mst)))))))))
