(ns mst.graph_theory
  (:require [clojure.set :as set]
            [mst.kdtree :as kdtree])
  (:use [clojure.data.priority-map]))

(defn distance ^Double [points ^Long u ^Long v]
  (letfn [(sq ^Double [^Double x] (* x x))]
    (let [abc (nth points u)
          ^Double a (nth abc 0)
          ^Double b (nth abc 1)
          ^Double c (nth abc 2)
          xyz (nth points v)
          ^Double x (nth xyz 0)
          ^Double y (nth xyz 1)
          ^Double z (nth xyz 2)]
      (+ (sq (- a x)) (sq (- b y)) (sq (- c z))))))

;; Relative Neighborhood Graph.
(defn RNG [points kdtree k]
  (letfn [(check-edge [^Long p ^Long q ^Double drq hood]
            ;; For all r, see if d(r,p) < d(p,q) and d(r,q) < d(p,q).
            ;; If either is true, then the edge (p,q) is not a member
            ;; of the RNG.
            (loop [hood (disj hood p q)]
              (if (not (empty? hood))
                                        ; not done checking, so check
                (let [^Long r (first hood)]
                  (if (and (< (distance points p r) drq)
                           (< (distance points q r) drq))
                                        ; failed this test
                    nil
                                        ; passed this one test
                    (recur (rest hood))))
                                        ; done checking, passed all of the tests
                #{p q})))
          ;; Report all RNG edges (u,v) where u < v for some given u.
          (at-u [u]
            (let [hood (set (map :index (kdtree/query (nth points u) kdtree k)))
                  candidates (set (filter #(< u %) hood))]
                                        ; for every v near u where u < v
              (loop [candidates candidates edges (list)]
                (if (not (empty? candidates))
                                        ; check to see if (u,v) is in the RNG
                  (let [v (first candidates)
                        d (distance points u v)]
                    (recur (disj candidates v) (conj edges (check-edge u v d (disj hood u v)))))
                                        ; return the found edges
                  (remove nil? edges)))))]
    (set (mapcat identity (pmap at-u (range (count points)))))))

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
