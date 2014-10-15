(defproject sr33 "0.99-SNAPSHOT"
  :description "A program and library for surface reconstruction."
  :url "http://daystrom-data-concepts.com/SR/"
  :license {:name "The BSD 3-Clause License"
            :url "http://opensource.org/licenses/BSD-3-Clause"}
  :dependencies [[org.clojure/clojure "1.7.0-alpha2"]
                 [clojure-complete "0.2.4"]
                 [org.clojure/tools.nrepl "0.2.3"]
                 [cider/cider-nrepl "0.8.0-SNAPSHOT"]
                 [org.clojure/data.priority-map "0.0.5"]
                 [org.clojure/core.memoize "0.5.6"]]
  :source-paths ["src"]
  :omit-source true
  :main sr33.core
  :jvm-opts ["-XX:+AggressiveOpts"]
  :aot [sr33.core sr33.reconstruct sr33.graph_theory sr33.kdtree])
