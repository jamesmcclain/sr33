(ns mst.kdtree
  (:require [quil.core :as q]))

(defn kdtree-leaf [xyz tree queue]
  (let [dist (apply q/dist (concat xyz (get tree :xyz)))
        entry (assoc tree :dist dist)
        n (count queue)]
    (letfn [(dist< [x y] (< (get x :dist) (get y :dist)))]
      (take n (sort dist< (conj queue entry))))))

;; This macro probably does not need to be one, but meh.
(defmacro kdtree-inner [xyz tree queue stretch this that]
  `(let [queue# (kdtree-query-aux ~xyz (get ~tree ~this) ~queue)
         worst# (reduce max (map :dist queue#))]
     (if (<= worst# ~stretch)
       queue#
       (kdtree-query-aux ~xyz (get ~tree ~that) queue#))))

(defn kdtree-query-aux [xyz tree queue]
  (cond
                                        ; leaf
   (= (get tree :type) :leaf) (kdtree-leaf xyz tree queue)
                                        ; inner
   (= (get tree :type) :inner)
   (let [stretch (- (nth xyz (:d tree)) (:split tree))]
     (if (<= stretch 0)
       (kdtree-inner xyz tree queue (- stretch) :left :right)
       (kdtree-inner xyz tree queue stretch :right :left)))))

(defn query [xyz tree k]
  (letfn [(fun-in-the-sun [] {:dist Double/POSITIVE_INFINITY :index -1})]
    (kdtree-query-aux xyz tree (repeatedly k fun-in-the-sun))))

(defn build
                                        ; just points
  ([points]
     (let [dims (map count points)
           min-dim (reduce min dims)
           max-dim (reduce max dims)]
       (if (= min-dim max-dim)
         (build (map list points (range)) 0 min-dim))))
                                        ; augmented points and dimensions
  ([points d ds]
     (let [n (count points)]
       (if (<= n 1)
                                        ; if done
         {:type :leaf
          :xyz (first (first points))
          :index (second (first points))}
                                        ; if not done
         (letfn [(P [x] (nth (first x) d))
                 (P< [x y] (< (P x) (P y)))]
           (let [points (sort P< points)
                 left (take (/ n 2) points)
                 right (drop (/ n 2) points)
                 split (/ (+ (P (last left)) (P (first right))) 2)]
             {:type :inner
              :d d :split split
              :left (build left (mod (inc d) ds) ds)
              :right (build right (mod (inc d) ds) ds)}))))))
