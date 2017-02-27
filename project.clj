(defproject clojure2d "0.0.1-SNAPSHOT"
  :description "Creative coding / glitch library backed by Java2D"
  :plugins [[refactor-nrepl "2.3.0-SNAPSHOT"]
            [cider/cider-nrepl "0.15.0-SNAPSHOT"]
            [lein-ancient "0.6.10"]
            [lein-kibit "0.1.3"]
            [jonase/eastwood "0.2.3"]
            [lein-bikeshed "0.4.1"]
            [venantius/yagni "0.1.4"]
            [lein-cljfmt "0.5.6"]
            [lein-deps-tree "0.1.2"]
            [lein-marginalia "0.9.0"]
            [com.jakemccrary/lein-test-refresh "0.18.1"]
            [lein-codox "0.10.3"]]
  :url "https://github.com/Clojure2D/clojure2d"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"] 
                 [net.jafama/jafama "2.1.0"]
                 [org.apache.commons/commons-math3 "3.6.1"]
                 [com.flowpowered/flow-noise "1.0.0"]
                 [com.sudoplay.joise/joise "1.0.5"]
		 [de.sciss/jwave "1.0.3"]
                 [criterium "0.4.4"]
                 [primitive-math "0.1.5"]]
  :resource-path "resources/"
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
