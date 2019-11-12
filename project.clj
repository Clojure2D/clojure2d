(defproject clojure2d "1.3.0-SNAPSHOT"
  :description "Creative coding / glitch library backed by Java2D"
  :url "https://github.com/Clojure2D/clojure2d"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [generateme/fastmath "1.5.0-alpha1"]
                 [org.apache.xmlgraphics/batik-transcoder "1.11"] ;; SVG
                 ;; [org.bytedeco/javacv-platform "1.4.1"]
                 ]
  :resource-path "resources/"
  :java-source-paths ["src"]
  :javac-options ["-target" "1.8" "-source" "1.8"]
  :prep-tasks [["compile" "fastmath.vector"] "javac"]
  :scm {:name "git"
        ::url "https://github.com/Clojure2D/clojure2d.git"}
  :repl-options {:timeout 120000}
  :target-path "target/%s"
  :profiles {:dev-codox {:codox {:source-uri "https://github.com/Clojure2D/clojure2d/blob/master/{filepath}#L{line}"
                                 :output-path "docs/codox/"}}
             :uberjar {:aot :all}})
