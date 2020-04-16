(ns cljdoc-specter-support.core
  (:require [cljs.analyzer.api :as ana]
            [clojure.java.io :as io])
  (:gen-class))

(defn -main
  "Highlight a crash of cljs.analyzer.api/analyze-file on the file specter.cljc"
  []
  (let [file (io/file "resources/specter-1.1.3/com/rpl/specter.cljc")]
    (ana/no-warn
     (ana/analyze-file file))))
