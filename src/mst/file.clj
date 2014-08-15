(ns mst.file
  (:require [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.string :as string]))

;; Squeeze the points down to fit into the box [-1,1]^3.
(defn boxify [points]
  (letfn [(column [n ps] (map #(nth % n) ps))]
    (let [mins (list
                (reduce min (column 0 points))
                (reduce min (column 1 points))
                (reduce min (column 2 points)))
          maxs (list
                (reduce max (column 0 points))
                (reduce max (column 1 points))
                (reduce max (column 2 points)))
          all-min (reduce min mins)
          all-max (reduce max maxs)
          half (/ (- all-max all-min) 2)
          points (map #(map - % (repeat all-min)) points)
          points (map #(map / % (repeat half)) points)
          points (map #(map - % (repeat 1)) points)]
      (vec (map vec points)))))

;; Load an "OFF" format file.
(defn load-off [filename]
  (with-open [r (io/reader filename)]
    (let [lines (line-seq r)
          off? (boolean (re-find #"^OFF$" (nth lines 0)))
          n (Long/parseLong (first (string/split (nth lines 1) #"\s+")))]
      (loop [points (vector) lines (take n (drop 2 lines))]
        (if (empty? lines)
          points
          (let [point (vec (map #(Double/parseDouble %) (string/split (first lines) #"\s+")))]
            (recur (conj points point) (rest lines))))))))

;; Load a Wavefront OBJ format file.
(defn load-obj [filename]
  (with-open [r (io/reader filename)]
    (letfn [(vertex? [line] (re-find #"^v\s" line))
            (vertex-string [v] (take 3 (drop 1 (string/split v #"\s+"))))
            (numerical-vertex [vs] (vec (map #(Double/parseDouble %) vs)))
            (line-to-point [vertex-line] (numerical-vertex (vertex-string vertex-line)))]
      (loop [points (vector) lines (line-seq r)]
        (if (empty? lines)
          points
          (let [line (first lines)]
            (if (vertex? line)
              (recur (conj points (line-to-point line)) (rest lines))
              (recur points (rest lines)))))))))

;; Write a Wavefront OBJ format file.
(defn save-obj [points surface filename]
  (binding [*out* (java.io.FileWriter. filename)]
    (letfn [(triangle? [face] (= 3 (count face)))
            (quad? [face] (= 4 (count face)))
            (bump [face] (map inc face))
            (print-triangle [face]
              (let [a (nth face 0)
                    u (nth face 1)
                    b (nth face 2)]
                (println "f" a u b)))
            (print-quad [face]
              (let [a (nth face 0)
                    u (nth face 1)
                    b (nth face 2)
                    v (nth face 3)]
                (println "f" a u b)
                (println "f" a v b)))]
      (doseq [point points]
        (println "v" (nth point 0) (nth point 1) (nth point 2)))
      (doseq [face (map bump surface)]
        (cond
         (triangle? face) (print-triangle face)
         (quad? face) (print-quad face))))))

(defn dotty [edges hood filename]
  (binding [*out* (java.io.FileWriter. filename)]
    (letfn [(in-hood? [edge] (set/subset? edge hood))]
      ;; https://en.wikipedia.org/wiki/DOT_language
      (println "graph graphname {")
      (doseq [edge (filter in-hood? edges)]
        (println "\t" (first edge) "--" (second edge) ";"))
      (println "}"))))
