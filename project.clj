(defproject cljs-6502 "0.1.0-SNAPSHOT"
  :description "A 6502 Emulator, Assembler, and Disassembler in pure Clojurescript."
  :source-path "src/clj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :plugins [[lein-cljsbuild "0.3.0"]]
  :hooks [leiningen.cljsbuild]
  :cljsbuild {:builds [{:source-paths ["src/cljs"]
                        :jar true
                        :compiler {:libs ["goog/dom/query.js"]
                                   :pretty-print true
                                   :output-dir ".cljsbuild/6502"
                                   :output-to "public/6502.js"}}]})
