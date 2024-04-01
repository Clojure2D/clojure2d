(defproject clojure2d "1.4.6-SNAPSHOT"
  :description "Creative coding / glitch library backed by Java2D"
  :url "https://github.com/Clojure2D/clojure2d"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [generateme/fastmath "2.4.0" :exclusions [com.github.haifengl/smile-mkl]]
                 [com.scrtwpns/mixbox "2.0.0"]
                 [org.apache.xmlgraphics/batik-transcoder "1.17"]]
  :resource-path "resources/"
  :java-source-paths ["src"]
  :javac-options ["--release" "8"]
  :prep-tasks [["compile" "fastmath.vector"] "javac"]
  :scm {:name "git"
        ::url "https://github.com/Clojure2D/clojure2d.git"}
  :repl-options {:timeout 120000}
  :target-path "target/%s"
  :profiles {:1.10 {:dependencies [[org.clojure/clojure "1.10.3"]]}
             :eastwood {:plugins [[jonase/eastwood "1.4.2"]]
                        :eastwood {:add-linters [:performance :boxed-math]
                                   :exclude-namespaces [notebooks.color]}}
             :dev {:source-paths ["dev"]
                   :dependencies [[io.github.nextjournal/clerk "0.13.842"]]}
             :dev-codox {:codox {:source-uri "https://github.com/Clojure2D/clojure2d/blob/master/{filepath}#L{line}"
                                 :output-path "docs/codox/"}}
             :uberjar {:aot :all}})
