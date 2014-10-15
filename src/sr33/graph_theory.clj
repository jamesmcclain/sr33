(ns ^{:author "James McClain <jwm@daystrom-data-concepts.com>"}
  sr33.graph_theory
  (:require [clojure.set :as set]
            [clojure.core.memoize :as memo]
            [clojure.core.reducers :as r]
            [sr33.kdtree :as kdtree])
  (:use [clojure.data.priority-map]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

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
(defn RNG [points n neighborhood-of]
  (let [distance (partial distance points)]
    (letfn [(edge? [p q]
              (let [dpq (distance p q)]
                (loop [R (set/union (neighborhood-of p) (neighborhood-of q))]
                  (cond
                   (empty? R) true ; done
                   (and (< (distance p (first R)) dpq)
                        (< (distance (first R) q) dpq))
                   false ; failed
                   :else (recur (rest R))))))
            (edges-at-p [p]
              (for [q (neighborhood-of p) :when (and (> q p) (edge? p q))] #{p q}))]
      (r/foldcat (r/mapcat edges-at-p (into [] (range n)))))))

;; CLRS page 595.
(defn Dijkstra [adjlist hood s]
  (loop [Q (priority-map s 0) dist (hash-map s 0) pi (hash-map s -1)]
    (let [[u udist] (peek Q)
          u (if (nil? u) -1 u)
          udist (if (nil? udist) (dec Long/MAX_VALUE) udist)
          udist+1 (+ udist 1)]
      (cond
       (empty? Q) [dist pi] ; done searching, function returns [dist pi]
       :else ; update Q and dist
       (let [[Q dist pi]
             (loop [Adju (filter hood (get adjlist u #{})) Q (pop Q) dist dist pi pi]
               (let [v (first Adju)
                     vdist (get dist v Long/MAX_VALUE)]
                 (cond
                  (or (empty? Adju) (nil? v) (nil? vdist)) [Q dist pi] ; all neighbors checked, done
                  (> vdist udist+1)
                  (recur (rest Adju) (assoc Q v udist+1) (assoc dist v udist+1) (assoc pi v u)) ; relax
                  :else ; next neighbor
                  (recur (rest Adju) Q dist pi))))]
         (recur Q dist pi))))))
