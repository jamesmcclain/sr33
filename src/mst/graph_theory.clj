(ns mst.graph_theory
  (:require [clojure.set :as set]
            [mst.kdtree :as kdtree])
  (:use [clojure.data.priority-map]))

(defn distance ^double [points ^long u ^long v]
  (letfn [(sq ^double [^double x] (* x x))]
    (let [abc (nth points u)
          ^double a (nth abc 0)
          ^double b (nth abc 1)
          ^double c (nth abc 2)
          xyz (nth points v)
          ^double x (nth xyz 0)
          ^double y (nth xyz 1)
          ^double z (nth xyz 2)]
      (+ (sq (- a x)) (sq (- b y)) (sq (- c z))))))

;; Relative Neighborhood Graph.
(defn RNG [points index-set kdtree fudge k]
  (letfn [(RNG-edge? [^long p ^long q ^double drq hood]
            ;; For all r, see if d(r,p) < d(p,q) and d(r,q) < d(p,q).
            ;; If either is true, then the edge (p,q) is not a member
            ;; of the RNG.
            (loop [hood hood]
              (if (not (empty? hood))
                                        ; not done checking, so check
                (let [^long r (first hood)]
                  (if (and (< (* fudge (distance points p r)) drq)
                           (< (* fudge (distance points q r)) drq))
                                        ; failed this test
                    false
                                        ; passed this one test
                    (recur (rest hood))))
                                        ; done checking, passed all of the tests
                true)))
          (RNG-edges-at-u [u]
            (let [near-u (set (map :index (kdtree/query (nth points u) kdtree k)))
                  hood (set/intersection index-set near-u)]
              ;; XXX make sure that hood is correct (don't remove v <= u).
              (for [v hood :when
                    (and (< u v) (RNG-edge? u v (distance points u v) (disj hood u v)))]
                #{u v})))]
    (disj (set (mapcat identity (pmap RNG-edges-at-u index-set))) nil)))

;; CLRS page 595.
(defn Dijkstra [adjlist hood s]
  (loop [Q (priority-map s 0) dist (hash-map s 0)]
    (let [[^Long u ^Long udist] (peek Q)
          ^Long udist+1 (if (not (nil? udist)) (+ udist 1) Long/MAX_VALUE)]
      (cond
                                        ; t is unreachable from s
       (or (empty? Q) (nil? u) (nil? udist)) dist
                                        ; update Q and dist
       :else
       (let [[Q dist]
             (loop [Adju (filter hood (get adjlist u)) Q (pop Q) dist dist]
               (let [^Long v (first Adju)
                     ^Long vdist (get dist v Long/MAX_VALUE)]
                 (cond
                                        ; all neighbors checked, done
                  (or (empty? Adju) (nil? v) (nil? vdist)) [Q dist]
                                        ; relax
                  (> vdist udist+1) (recur (rest Adju) (assoc Q v udist+1) (assoc dist v udist+1))
                                        ; next neighbor
                  :else (recur (rest Adju) Q dist))))]
         (recur Q dist))))))
