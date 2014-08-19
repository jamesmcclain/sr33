(ns mst.file
  (:require [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [mst.reconstruct :as recon]))

;; Squeeze the points down to fit into the box [-1,1]^3.
(defn boxify [points]
  (letfn [(column [n ps] (map #(nth % n) ps))]
    (let [m (reduce min (flatten points))
          M (reduce max (flatten points))
          middle (/ (- M m) 2)
          points (map #(map - % (repeat m)) points)
          points (map #(map / % (repeat middle)) points)
          points (map #(map - % (repeat 1)) points)]
      (vec (map #(apply vector-of :double %) points)))))

;; Load an "OFF" format file.
(defn load-off [filename]
    (with-open [r (io/reader filename)]
      (letfn [(vertex? [line] (= 3 (count (string/split line #"\s+"))))
              (get-vertex [vertex] (map #(Double/parseDouble %) (string/split vertex #"\s+")))]
        (loop [lines (drop 2 (line-seq r)) vertices (list)]
          (cond
           (empty? lines) (boxify vertices)
           (vertex? (first lines)) (recur (rest lines) (conj vertices (get-vertex (first lines))))
           :else (recur (rest lines) vertices))))))

;; Load a Wavefront OBJ format file.
(defn load-obj [filename]
    (with-open [r (io/reader filename)]
      (letfn [(vertex? [line] (re-find #"^v\s" line))
              (get-vertex [vertex] (map #(Double/parseDouble %) (drop 1 (string/split vertex #"\s+"))))]
        (loop [lines (line-seq r) vertices (list)]
          (cond
           (empty? lines) (boxify vertices)
           (vertex? (first lines)) (recur (rest lines) (conj vertices (get-vertex (first lines))))
           :else (recur (rest lines) vertices))))))

;; Write a Wavefront OBJ format file.
(defn save-obj [points surface filename]
  (binding [*out* (java.io.FileWriter. filename)]
    (letfn [(print-point [point]
              (let [x (nth point 0)
                    y (nth point 1)
                    z (nth point 2)]
                (println "v" x y z)))
            (print-face [face]
              (let [a (inc (nth face 0))
                    b (inc (nth face 1))
                    c (inc (nth face 2))]
                (println "f" a b c)))]
      (doseq [point points]
        (print-point point))
      (doseq [face surface]
        (print-face face)))))

(defn save-povray [points surface name filename]
  (binding [*out* (java.io.FileWriter. filename)]
    (letfn [(print-point [point]
              (let [x (nth point 0)
                    y (nth point 1)
                    z (nth point 2)]
                (println ", <" x "," y "," z ">")))
            (print-face [face]
              (let [a (nth face 0)
                    b (nth face 1)
                    c (nth face 2)]
                (println ", <" a "," b "," c ">")))]
      (println "#declare" name "= mesh2 {")

      (println "vertex_vectors {" (count points))
      (doseq [point points]
        (print-point point))
      (println "}")

      (println "face_indices {" (count surface))
      (doseq [face surface]
        (print-face face))
      (println "}")

      (println "}"))))

;; Write a neighborhood in DOT format.
(defn dotty [edges hood filename]
  (binding [*out* (java.io.FileWriter. filename)]
    (letfn [(in-hood? [edge] (set/subset? edge hood))]
      ;; https://en.wikipedia.org/wiki/DOT_language
      (println "graph graphname {")
      (doseq [edge (filter in-hood? edges)]
        (println "\t" (first edge) "--" (second edge) ";"))
      (println "}"))))
