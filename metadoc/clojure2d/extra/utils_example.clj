(ns clojure2d.extra.utils-example
  (:require [metadoc.examples :refer :all]
            [clojure2d.extra.utils :refer :all]
            [clojure2d.color :as c]))

(add-examples show-palette
  (example "Usage" {:evaluate? false} (show-palette [:red :green :blue])))

(add-examples show-color
  (example "Usage" {:evaluate? false} (show-color :yellow)))

(add-examples show-gradient
  (example "Usage" {:evaluate? false} (show-gradient (c/gradient-easing :yellow :darkblue))))
