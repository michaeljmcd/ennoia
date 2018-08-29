(defproject ennoia "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :hooks [leiningen.cljsbuild]
  :dependencies [[org.clojure/clojure "1.8.0"]
        [org.clojure/clojurescript "1.10.339"]
                 [reagent  "0.7.0"]
                 [re-frame "0.10.5"]]
  :plugins [[lein-cljsbuild "1.1.7"]
           [lein-figwheel  "0.5.14"]
  ]
  :resource-paths ["resources"]
  :cljsbuild {
    :builds [{
        :id "dev"
        ; The path to the top-level ClojureScript source directory:
        :source-paths ["src"]
        ; The standard ClojureScript compiler options:
        ; (See the ClojureScript compiler documentation for details.)
        :compiler {
          :main ennoia.app
          :warnings true
          :elide-asserts true
          :output-to "resources/public/app.js"
          :output-dir "resources/public/js/compiled/out"
          :asset-path "js/compiled/out" ;; <--- relative URL of output-dir
          :pretty-print true}}
    {
        :id "desktop"
        ; The path to the top-level ClojureScript source directory:
        :source-paths ["src"]
        ; The standard ClojureScript compiler options:
        ; (See the ClojureScript compiler documentation for details.)
        :compiler {
          :main ennoia.electron-host
          :warnings true
          :elide-asserts true
          :output-to "package/index.js"
          :target :nodejs
          :optimizations :simple
          :pretty-print true}}
    ]})
