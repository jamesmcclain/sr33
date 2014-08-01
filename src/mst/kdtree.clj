(ns mst.kdtree)

(defn dist [xyz abc]
  (letfn [(square [x] (* x x))]
    (Math/sqrt (reduce + (map (comp square -) xyz abc)))))

(defn kdtree-leaf [xyz tree queue]
  (let [distance (dist xyz (get tree :xyz))
        entry (assoc tree :dist distance)
        n (count queue)]
    ;; XXX inefficient way to insert into already-sorted list
    (take n (sort-by :dist < (conj queue entry)))))

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
