(defproject mst "0.1.0-SNAPSHOT"
  :description "A program and library for surface reconstruction."
  :url "http://daystrom-data-concepts.com/SR/"
  :license {:name "The BSD 3-Clause License"
            :url "http://opensource.org/licenses/BSD-3-Clause"}
  :dependencies [[org.clojure/clojure "1.7.0-alpha1"]
                 [org.clojure/tools.nrepl "0.2.3"]
                 [org.clojure/data.priority-map "0.0.5"]
                 [org.clojure/core.memoize "0.5.6"]
                 [clojure-complete "0.2.3"]]
  :source-paths ["src"]
  :omit-source true
  :main mst.core
  :jvm-opts ["-XX:+AggressiveOpts"]
  :aot [mst.core mst.reconstruct mst.graph_theory mst.kdtree])
