(defproject mst "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.priority-map "0.0.5"]
                 [quil "2.2.0"]]
  :source-paths ["src"]
  ;; :omit-source true
  :main mst.core
  ;; :jvm-opts ["-XX:+AggressiveOpts" "-Xmx2g"]
  :aot [mst.core])
