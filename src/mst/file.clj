(ns mst.file
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

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
          points (map #(map - % (list all-min all-min all-min)) points)
          points (map #(map / % (list half half half)) points)
          points (map #(map + % '(-1 -1 -1)) points)]
      (vec (map vec points)))))

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

(defn load-obj [filename]
  (with-open [r (io/reader filename)]
    (letfn [(vertex? [line] (re-find #"^v\s" line))
            (vertex-string [v] (take 3 (drop 1 (string/split v #"\s+"))))
            (numerical-vertex [vs] (vec (map #(Double/parseDouble %) vs)))
            (to-point [v] ((comp numerical-vertex vertex-string) v))]
      (loop [points (vector) lines (line-seq r)]
        (if (empty? lines)
          points
          (let [line (first lines)]
            (if (vertex? line)
              (recur (conj points (to-point line)) (rest lines))
              (recur points (rest lines)))))))))
