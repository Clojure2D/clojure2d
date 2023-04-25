(defproject clojure2d "1.4.5"
  :description "Creative coding / glitch library backed by Java2D"
  :url "https://github.com/Clojure2D/clojure2d"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [generateme/fastmath "2.2.0" :exclusions [com.github.haifengl/smile-mkl]]
                 [com.scrtwpns/mixbox "2.0.0"]
                 [org.apache.xmlgraphics/batik-transcoder "1.16"]]
  :resource-path "resources/"
  :java-source-paths ["src"]
  ;; :javac-options ["-target" "1.8" "-source" "1.8"]
  :javac-options ["--release" "8"]
  :prep-tasks [["compile" "fastmath.vector"] "javac"]
  :scm {:name "git"
        ::url "https://github.com/Clojure2D/clojure2d.git"}
  :repl-options {:timeout 120000}
  :target-path "target/%s"
  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[io.github.nextjournal/clerk "0.13.842"]]}
             :dev-codox {:codox {:source-uri "https://github.com/Clojure2D/clojure2d/blob/master/{filepath}#L{line}"
                                 :output-path "docs/codox/"}}
             :uberjar {:aot :all}})
