(defproject cljs-6502 "0.1.0-SNAPSHOT"
  :description "A 6502 Emulator, Assembler, and Disassembler in pure Clojurescript."
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [compojure "1.1.5"]]
  :plugins [[lein-cljsbuild "0.3.0"]
            [lein-ring "0.8.3"]]
  :source-paths ["src/clj"]
  :ring {:handler clj-6502.core/handler}
  :cljsbuild {:builds [{:source-paths ["src/cljs"]
                        :compiler {:libs ["goog/dom/query.js"]
                                   :pretty-print true
                                   :output-dir ".cljsbuild/6502"
                                   :output-to "public/6502.js"}}]})
