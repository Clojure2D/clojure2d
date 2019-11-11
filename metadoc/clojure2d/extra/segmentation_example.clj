(ns clojure2d.extra.segmentation-example
  (:require [clojure2d.extra.segmentation :refer :all]
            [clojure2d.pixels :as p]
            [metadoc.examples :refer :all]))

(add-examples segment-pixels
  (example-session "Usage"
    (count (segment-pixels (p/load-pixels "docs/cockatoo.jpg") 0))
    (nth (segment-pixels (p/load-pixels "docs/cockatoo.jpg") 0 {:min-size 2 :threshold 40}) 5)))
