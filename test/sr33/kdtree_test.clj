(ns sr33.kdtree-test
  (:require [sr33.kdtree :as kdtree])
  (:use clojure.test))

(defn generate-point [ds]
  (repeat ds (rand)))

(defn generate-random-points [ds n]
  (let [points (repeat n (generate-point ds))
        aug-points (map #(list %1 (long %2)) points (range))]
    aug-points))

;; Naive distance query
(defn naive-query [query-point points]
  (letfn [(point-to-leaf [[point index]]
            (hash-map :dist (kdtree/distance query-point point)
                      :type :leaf
                      :xyz point
                      :index index))]
    (sort-by :dist (map point-to-leaf points))))

;; Test kd-tree construction and queries
(deftest build-query-test
  (let [ds (+ 2 (rand-int 33))
        n (+ 2 (rand-int 107))  
        aug-points (generate-random-points ds n)
        kdtree (kdtree/build aug-points 0 ds)]
    (testing "kd-tree construction and queries."
      (dotimes [_ 33]
        (let [query-point (generate-point ds)
              naive (naive-query query-point aug-points)
              i (rand-int n)]
          ;; make sure the distances to the ith answer are the same
          ;; (equidistant points can appear in any order, so testing
          ;; for point equality will lead to spurious failures).
          (is (= (:dist (nth naive i))
                 (:dist (nth (kdtree/query query-point kdtree (inc i)) i)))))))))
