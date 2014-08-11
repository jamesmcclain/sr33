(ns mst.core
  (:require [clojure.set :as set]
            [clojure.java.io :as io]
            [mst.reconstruct :as recon]
            [mst.kdtree :as kdtree]
            [mst.graph_theory :as theory]
            [mst.file :as file])
  (:use [clojure.tools.nrepl.server :only [start-server stop-server]])
  (:gen-class))

(defn -main []
  (print "Α -")
  (defonce ^:dynamic *repl-server* (start-server :port 4005))
  (println " Ω"))
