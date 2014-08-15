(defproject mst "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.nrepl "0.2.3"]
                 [clojure-complete "0.2.3"]
                 [org.clojure/data.priority-map "0.0.5"]
                 [org.clojure/core.memoize "0.5.6"]]
  :source-paths ["src"]
  ;; :omit-source true
  :main mst.core
  :jvm-opts ["-XX:+AggressiveOpts"]
  :aot [mst.core])
