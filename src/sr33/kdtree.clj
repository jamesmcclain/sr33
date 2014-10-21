(ns ^{:author "James McClain <jwm@daystrom-data-concepts.com>"}
  sr33.kdtree)

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; Euclidean distance.
(defn distance [xyz abc]
  (letfn [(square [^double x] (double (* x x)))]
    (Math/sqrt (reduce + (map (comp square -) xyz abc)))))

;; Sub-query on a leaf of a KD-tree.
(defn- kdtree-leaf [xyz tree queue]
  (let [dist (distance xyz (get tree :xyz))
        entry (assoc tree :dist dist)
        k (count queue)]
    ;; XXX probably inefficient way to insert into already-sorted list
    (take k (sort-by :dist < (conj queue entry)))))

;; Sub-query a non-leaf of a KD-tree.  This macro might not need to be a macro.
(defmacro kdtree-inner [xyz tree queue stretch this that]
  `(let [queue# (kdtree-query-aux ~xyz (get ~tree ~this) ~queue)
         worst# (double (reduce max (map :dist queue#)))]
     ;; ``worst'' is the distance from the query point to the kth
     ;; closest point that we know about so far.  ``stretch'' is the
     ;; distance from the query point to the boundary of the split.
     ;; If worst <= stretch, then we know that there is no profit in
     ;; searching the sibling.  If not, then the sibling must be
     ;; searched.
     (if (<= worst# ~stretch)
       queue#
       (kdtree-query-aux ~xyz (get ~tree ~that) queue#))))

;; Recursively query a KD-tree.
(defn- kdtree-query-aux [xyz tree queue]
  (cond
                                        ; leaf
   (= (get tree :type) :leaf) (kdtree-leaf xyz tree queue)
                                        ; inner
   (= (get tree :type) :inner)
   ;; ``stretch'' is the distance from the query point to the boundary
   ;; of the split between the left and right sub-trees.  If it is
   ;; negative, go left.  If it is positive, go right.
   (let [stretch (- (double (nth xyz (:d tree))) (double (:split tree)))]
     (if (<= stretch 0)
       (kdtree-inner xyz tree queue (- stretch) :left :right)
       (kdtree-inner xyz tree queue stretch :right :left)))))

;; Query a KD-tree.
(defn query [xyz tree k]
  (kdtree-query-aux xyz tree (take k (repeat {:dist Double/POSITIVE_INFINITY :index -1}))))

(defn build
  ;; Takes just points as input.  Augments the points with an index,
  ;; determines the dimensionality of the data, and calls the 3-atic
  ;; version of build to build the kd-tree.
  ([points]
     (let [dims (map count points)
           min-dim (reduce min dims)
           max-dim (reduce max dims)]
       (if (= min-dim max-dim) ; if all points have the same number of coordinates
         (build (map #(list %1 (long %2)) points (range)) 0 min-dim)))) ; give each point an index and build tree
  ;; Takes augmented points, current dimension, and number of
  ;; dimensions.  Returns a kd-tree over the points.
  ([aug-points ^long d ^long ds]
     (let [n (count aug-points)]
       (if (<= n 1)
                                        ; a leaf
         {:type :leaf
          :xyz (first (first aug-points))
          :index (second (first aug-points))}
                                        ; an inner node
         (letfn [(P [x] (nth (first x) d))] ; P is a projection
           (let [next-d (mod (inc d) ds)
                 aug-points (sort-by P < aug-points)
                 left (take (/ n 2) aug-points)
                 right (drop (/ n 2) aug-points)
                 split (/ (+ (double (P (last left))) (double (P (first right)))) 2.0)]
             {:type :inner
              :d d :split split
              :left (build left next-d ds)
              :right (build right next-d ds)}))))))
