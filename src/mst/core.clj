(ns mst.core
  (:require [clojure.java.io :as io]
            [mst.reconstruct :as recon]
            [mst.kdtree :as kdtree]
            [mst.graph_theory :as theory]
            [mst.file :as file]
            [mst.grade :as grade])
  (:use [clojure.tools.nrepl.server :only [start-server stop-server]])
  (:gen-class))

(defn reconstruct [filename k retries]
  (let [points
        (cond
         (re-find #"\.obj$" filename) (file/load-obj filename)
         (re-find #"\.off$" filename) (file/load-off filename)
         :else (throw (Exception. "?")))
        surface (time (recon/compute-surface points k retries))]
    (grade/grade-surface surface)
    (file/save-obj points surface (str filename ".recon." k "." retries ".obj"))
    (file/save-povray points surface "reconstruction" (str filename ".recon." k "." retries ".inc"))))

(defn -main []
  (print "Α -")
  (defonce ^:dynamic *repl-server* (start-server :port 4005))
  (println " Ω"))
