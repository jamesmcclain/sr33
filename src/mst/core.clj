(ns mst.core
  (:require [clojure.java.io :as io]
            [mst.reconstruct :as recon]
            [mst.kdtree :as kdtree]
            [mst.graph_theory :as theory]
            [mst.file :as file]
            [mst.grade :as grade])
  (:use [clojure.tools.nrepl.server :only [start-server stop-server]])
  (:gen-class))

(defn finished []
  (try
    (let [loader (.getContextClassLoader (Thread/currentThread))
          stream (.getResourceAsStream loader "HOTPOCKETS-TAG.aiff")
          bufstream (java.io.BufferedInputStream. stream)
          sound (javax.sound.sampled.AudioSystem/getAudioInputStream bufstream)
          clip (javax.sound.sampled.AudioSystem/getClip)]
      (.open clip sound)
      (.setFramePosition clip 0)
      (.start clip)
      (Thread/sleep 2000)
      (.close clip))
    (catch Exception ex)))

(defn reconstruct [filename k retries & bell]
  (let [points
        (cond
         (re-find #"\.obj$" filename) (file/load-obj filename)
         (re-find #"\.off$" filename) (file/load-off filename)
         :else (throw (Exception. "?")))
        surface (time (recon/compute-surface points k retries))]
    (grade/grade-surface surface)
    (file/save-obj points surface (str filename ".recon." k "." retries ".obj"))
    (file/save-povray points surface "reconstruction" (str filename ".recon." k "." retries ".inc"))
    (if (not (empty? bell)) (finished))))

(defn mumbo []
  (println "
  Copyright (c) 2014, James McClain
  All rights reserved.")
  (println "
  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  \"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
  COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE."))

(defn -main []
  (defonce ^:dynamic *repl-server* (start-server :port 4005))
  (mumbo))
