(ns cljdoc-specter-support.core
  (:require [cljs.analyzer.api :as ana]
            [clojure.java.io :as io]
            [essential]
            )
  (:gen-class))

(defn -main
  "Highlight a crash of cljs.analyzer.api/analyze-file on the file essential.cljc"
  []

  (assert ((essential/simple) [:foo]))

  (let [file (io/file "src/main/essential.cljc")]
    (ana/no-warn
     (ana/analyze-file file))
    (prn "analyze-file passed")))
