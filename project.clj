(defproject cljdoc_specter_support "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/clojurescript "1.10.597"]
                 ;; Necessary add com.rpl/specter as a dependency. This allows
                 ;; cljs.analyzer.api/analyze-file to find the compilation
                 ;; dependencies from the "com.rpl.specter" name space.
                 [com.rpl/specter "1.1.3"]
                 ]
  :main ^:skip-aot cljdoc-specter-support.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
