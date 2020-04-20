(ns cljdoc-specter-support.core
  (:require [cljs.analyzer.api :as ana]
            [clojure.java.io :as io]
            [com.rpl.specter :as sp]
            )
  (:gen-class))

(defn -main
  "Highlight a crash of cljs.analyzer.api/analyze-file on the file specter.cljc"
  []

  ;; (assert
  ;;  (= (sp/select (sp/walker number?)
  ;;                {2 [1 2 [6 7]] :a 4 :c {:a 1 :d [2 nil]}})
  ;;     [2 1 2 6 7 4 1 2]))

  (let [file (io/file "src/lib/specter-1.1.3/com/rpl/specter.cljc")]
    (ana/no-warn
     (ana/analyze-file file))
    (prn "Success!")))
