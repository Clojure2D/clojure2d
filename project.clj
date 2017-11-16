(defproject clojure2d "0.0.5-SNAPSHOT"
  :description "Creative coding / glitch library backed by Java2D"
  :plugins [[refactor-nrepl "2.4.0-SNAPSHOT"]
            [cider/cider-nrepl "0.16.0-SNAPSHOT"]
            [lein-ancient "0.6.14"]
            [lein-kibit "0.1.6-beta2"]
            [lein-bikeshed "0.4.1"]
            [lein-deps-tree "0.1.2"]
            [lein-marginalia "0.9.1"]
            [lein-expectations "0.0.8"]
            [lein-autoexpect "1.9.0"]
            [com.jakemccrary/lein-test-refresh "0.21.0"]
            [lein-codox "0.10.3"]]
  :url "https://github.com/Clojure2D/clojure2d"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [
                 ;; [org.clojure/clojure "1.8.0"]
                 [org.clojure/clojure "1.9.0-RC1"]
                 [expectations "2.2.0-SNAPSHOT"]
                 [net.jafama/jafama "2.1.0"]
                 [org.apache.commons/commons-math3 "3.6.1"]
                 [com.flowpowered/flow-noise "1.0.0"]
                 [com.sudoplay.joise/joise "1.1.0"]
                 [net.littleredcomputer/sicmutils "0.10.0"] ;; to be removed
                                        ;	 [de.sciss/jwave "1.0.3"]
                 [criterium "0.4.4"]
                                        ; [org.bytedeco/javacv-platform "1.3.2"]
                 ]
  :resource-path "resources/"
  :codox {:output-path "docs/codox/"
          :exclude-vars nil}
  :java-source-paths ["src"]
  :repl-options {:timeout 120000}
  :target-path "target/%s"
  :jvm-opts ["-Xmx4096M"
                                        ;   "-Dcom.sun.management.jmxremote"
                                        ;   "-Dcom.sun.management.jmxremote.ssl=false"
                                        ;   "-Dcom.sun.management.jmxremote.authenticate=false"
                                        ;   "-Dcom.sun.management.jmxremote.port=43210"
             ]
  :marginalia {:javascript ["https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML"]}
  :profiles {:uberjar {:aot :all}})
