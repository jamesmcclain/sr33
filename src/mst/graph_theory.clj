(ns mst.graph_theory
  (:require [clojure.set :as set]
            [clojure.core.memoize :as memo]
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
      (+ (sq (- a x)) (+ (sq (- b y)) (sq (- c z)))))))

;; Relative Neighborhood Graph.  For all r, see if d(p,r) < d(p,q) and
;; d(r,q) < d(p,q).  If both are true, then the edge (p,q) is not a
;; member of the RNG, otherwise it is.
(defn RNG [points index-set neighborhood-of epsilon]
  (let [distance (partial distance points)]
    (letfn [(edge? [p q]
              (let [dpq (/ (distance p q) epsilon)]
                (loop [R (set/union (neighborhood-of p) (neighborhood-of q))]
                  (cond
                                        ; done
                   (empty? R) true
                                        ; failed
                   (and (< (distance p (first R)) dpq)
                        (< (distance (first R) q) dpq))
                   false
                   :else (recur (rest R))))))
            (edges-at-p [p]
              (for [q (remove #(<= p %) (neighborhood-of p)) :when (edge? p q)]
                #{p q}))]
      (apply concat (pmap edges-at-p index-set)))))

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
