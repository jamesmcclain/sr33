(ns ^{:author "James McClain <jwm@daystrom-data-concepts.com>"}
  sr33.grade
  (:require [clojure.set]
            [clojure.java.io :as io]
            [clojure.string :as string]))

;; Print out some statistics concerning the quality of the surface.
(defn grade-surface [surface]
  (letfn [(triangle-to-edges [tri]
            (let [tri (seq tri)
                  a (nth tri 0)
                  b (nth tri 1)
                  c (nth tri 2)]
              (list #{a b} #{b c} #{c a})))]
    (let [surface (set surface)
          edges-multiple (mapcat triangle-to-edges surface)
          edges-unique (set edges-multiple)
          points-unique (set (flatten (map seq edges-unique)))
          edge-counts (frequencies edges-multiple)
          count-counts (frequencies (map second edge-counts))]
      (println "vertices:\t" (count points-unique))
      (println "edges:\t\t" (count edges-unique) count-counts)
      (println "faces:\t\t" (count surface)))))

;; Load a previously-computed surface so that it can be graded.
(defn load-surface [filename regexp] 
    (with-open [r (io/reader filename)]
      (letfn [(face? [line] ; the regular expression is used to recognize faces
                (re-find regexp line))
              (get-face [face] ; the line is assumed to consist of something followed by three indices
                (map #(Long/parseLong %) (drop 1 (string/split face #"\s+"))))]
        (loop [lines (line-seq r) surface (list)]
          (cond
           (empty? lines) surface
           (face? (first lines)) (recur (rest lines) (conj surface (get-face (first lines))))
           :else (recur (rest lines) surface))))))

;; Grade a previously-computed surface stored in an .obj file.
(defn grade-obj [filename]
  (grade-surface (load-surface filename #"^f\s")))

;; Grade a previously-computed surface stored in an .off file.
(defn grade-off [filename]
  (grade-surface (load-surface filename #"^3\s")))
