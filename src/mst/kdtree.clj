(ns mst.kdtree)

;; Euclidean distance.
(defn dist [xyz abc]
  (letfn [(square [x] (* x x))]
    (Math/sqrt (reduce + (map (comp square -) xyz abc)))))

;; Sub-query on a leaf of a KD-tree.
(defn kdtree-leaf [xyz tree queue]
  (let [distance (dist xyz (get tree :xyz))
        entry (assoc tree :dist distance)
        n (count queue)]
    ;; XXX probably inefficient way to insert into already-sorted list
    (take n (sort-by :dist < (conj queue entry)))))

;; Sub-query a non-leaf of a KD-tree.  This macro might not need to be
;; a macro.
(defmacro kdtree-inner [xyz tree queue stretch this that]
  `(let [queue# (kdtree-query-aux ~xyz (get ~tree ~this) ~queue)
         worst# (reduce max (map :dist queue#))]
     (if (<= worst# ~stretch)
       queue#
       (kdtree-query-aux ~xyz (get ~tree ~that) queue#))))

;; Recursively query a KD-tree.
(defn kdtree-query-aux [xyz tree queue]
  (cond
                                        ; leaf
   (= (get tree :type) :leaf) (kdtree-leaf xyz tree queue)
                                        ; inner
   (= (get tree :type) :inner)
   ;; ``stretch'' is the distance from the worst answer to worst (kth)
   ;; answer to the query that has been found so far and the boundary
   ;; of the current split between the left and right sub-trees.
   (let [stretch (- (nth xyz (:d tree)) (:split tree))]
     (if (<= stretch 0)
       (kdtree-inner xyz tree queue (- stretch) :left :right)
       (kdtree-inner xyz tree queue stretch :right :left)))))

;; Query a KD-tree.
(defn query [xyz tree k]
  (kdtree-query-aux xyz tree (take k (repeat {:dist Double/POSITIVE_INFINITY :index -1}))))

(defn build
  ;; Points
  ([points]
     (let [dims (map count points)
           min-dim (reduce min dims)
           max-dim (reduce max dims)]
       (if (= min-dim max-dim)
         (build (map #(list %1 (long %2)) points (range)) 0 min-dim))))

  ;; Augmented points, current dimension, number of dimensions
  ([points d ds]
     (let [n (count points)]
       (if (<= n 1)
                                        ; a leaf
         {:type :leaf
          :xyz (first (first points))
          :index (second (first points))}
                                        ; an inner node
         (letfn [(P [x] (nth (first x) d))]
           (let [next-d (mod (inc d) ds)
                 points (sort-by P < points)
                 left (take (/ n 2) points)
                 right (drop (/ n 2) points)
                 split (/ (+ (P (last left)) (P (first right))) 2)]
             {:type :inner
              :d d :split split
              :left (build left next-d ds)
              :right (build right next-d ds)}))))))
