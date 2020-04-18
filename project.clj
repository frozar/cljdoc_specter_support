(defproject cljdoc_specter_support "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/clojurescript "1.10.597"]
                 ;; specter 1.1.3 dependencies
                 [riddley "0.1.12"]
                 ]
  :main ^:skip-aot cljdoc-specter-support.core
  :target-path "target/%s"
  :source-paths ["src/main" "src/lib/specter-1.1.3"]
  :profiles {:uberjar {:aot :all}})
