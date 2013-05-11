(defproject cljs-6502 "0.1.0"
  :description "A 6502 Emulator, Assembler, and Disassembler in pure Clojurescript."
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [com.cemerick/piggieback "0.0.4"]]
  :plugins [[lein-cljsbuild "0.3.0"]]
  :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
  :source-paths ["src/clj"]
  :cljsbuild {:builds [{:source-paths ["src/cljs"]
                        :compiler {:libs ["goog/dom/query.js"]
                                   :pretty-print true
                                   :output-dir ".cljsbuild/6502"
                                   :output-to "resources/public/6502.js"}}]})
