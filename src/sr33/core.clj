(ns ^{:author "James McClain <jwm@daystrom-data-concepts.com>"}
  sr33.core
  (:require [clojure.java.io :as io]
            [sr33.reconstruct :as recon]
            [sr33.kdtree :as kdtree]
            [sr33.graph_theory :as theory]
            [sr33.file :as file]
            [sr33.grade :as grade]
            [clojure.tools.nrepl.server :as nrepl-server]
            [cider.nrepl :refer (cider-nrepl-handler)])
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
      (Thread/sleep 2100)
      (.close clip))
    (catch Exception ex)))

(defn reconstruct [filename k & bell]
  (let [points
        (cond
         (re-find #"\.obj$" filename) (file/load-obj filename)
         (re-find #"\.off$" filename) (file/load-off filename)
         :else (throw (Exception. "?")))
        surface (time (recon/compute-surface points k))]
    (grade/grade-surface surface)
    (file/save-obj points surface (str filename ".recon." k ".obj"))
    (file/save-povray points surface "reconstruction" (str filename ".recon." k ".inc"))
    (if (not (empty? bell)) (finished))))

(defn mumbo []
  (println "
  Copyright (c) 2014, James McClain
  All rights reserved.

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
  (defonce ^:dynamic *repl-server* (nrepl-server/start-server :port 4005 :handler cider-nrepl-handler))
  (mumbo))
