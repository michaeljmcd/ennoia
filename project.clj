(defproject ennoia "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :hooks [leiningen.cljsbuild]
  :dependencies [[org.clojure/clojure "1.8.0"]
        [org.clojure/clojurescript "1.10.339"]]
  :plugins [[lein-cljsbuild "1.1.7"]]
  :cljsbuild {
    :builds [{
        ; The path to the top-level ClojureScript source directory:
        :source-paths ["src"]
        ; The standard ClojureScript compiler options:
        ; (See the ClojureScript compiler documentation for details.)
        :compiler {
          :main ennoia.core
          :output-to "package/index.js"
          :target :nodejs
          :output-dir "target"
          :optimizations :advanced
          :pretty-print true}}]})
