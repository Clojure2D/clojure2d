(defproject clojure2d "0.0.7-SNAPSHOT"
  :description "Creative coding / glitch library backed by Java2D"
  :plugins [[refactor-nrepl "2.4.0-SNAPSHOT"]
            [cider/cider-nrepl "0.17.0-SNAPSHOT"]
            [lein-ancient "0.6.15"]
            [lein-kibit "0.1.6"]
            [lein-bikeshed "0.5.0"]
            [lein-marginalia "0.9.1"]
            [com.jakemccrary/lein-test-refresh "0.22.0"]
            [lein-codox "0.10.3"]]
  :url "https://github.com/Clojure2D/clojure2d"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [net.jafama/jafama "2.3.1"]
                 [org.apache.commons/commons-math3 "3.6.1"]
                 [com.flowpowered/flow-noise "1.0.0"]
                 [com.sudoplay.joise/joise "1.1.0"]                 
                 [net.littleredcomputer/sicmutils "0.10.0"] ;; to be removed
                                        ;	 [de.sciss/jwave "1.0.3"]
                 [criterium "0.4.4"]
                 [codox-theme-rdash "0.1.2"]
                                        ; [org.bytedeco/javacv-platform "1.3.2"]
                 ]
  :resource-path "resources/"
  :codox {:themes [:rdash]
          :metadata {:doc/format :markdown}
          :output-path "docs/codox/"
          :source-uri "https://github.com/Clojure2D/clojure2d/blob/master/{filepath}#L{line}"
          :exclude-vars nil
          :doc-paths ["docs/tutorials/"]
          :html {:transforms [[:head] [:append [:script {:type "text/javascript",
                                                         :async ""
                                                         :src "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2/MathJax.js?config=TeX-MML-AM_CHTML"}]]]}}
  :java-source-paths ["src"]
  :scm {:name "git"
        ::url "https://github.com/Clojure2D/clojure2d.git"}
  :repl-options {:timeout 120000}
  :target-path "target/%s"
  :jvm-opts ["-Xmx4096M"
                                        ;   "-Dcom.sun.management.jmxremote"
                                        ;   "-Dcom.sun.management.jmxremote.ssl=false"
                                        ;   "-Dcom.sun.management.jmxremote.authenticate=false"
                                        ;   "-Dcom.sun.management.jmxremote.port=43210"
             ]
  :marginalia {:javascript ["https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2/MathJax.js?config=TeX-MML-AM_CHTML"]} ;; to remove
  :profiles {:uberjar {:aot :all}})
