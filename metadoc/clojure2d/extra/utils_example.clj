(ns clojure2d.extra.utils-example
  (:require [metadoc.examples :refer :all]
            [clojure2d.core :as core]
            [clojure2d.extra.utils :refer :all]
            [clojure2d.color :as c]))

(core/save (palette->image (c/palette 0)) "docs/images/utils/palatte.png")
(core/save (gradient->image (c/gradient :iq-1)) "docs/images/utils/gradient.png")
(core/save (color->image :blueviolet) "docs/images/utils/color.png")

(add-examples palette->image
  (example-image "Rendered palette" "../images/utils/palatte.png"))

(add-examples gradient->image
  (example-image "Rendered gradient" "../images/utils/gradient.png"))

(add-examples color->image
  (example-image "Rendered color" "../images/utils/color.png"))

(add-examples show-palette
  (example "Usage" {:evaluate? false} (show-palette [:red :green :blue])))

(add-examples show-color
  (example "Usage" {:evaluate? false} (show-color :yellow)))

(add-examples show-gradient
  (example "Usage" {:evaluate? false} (show-gradient (c/gradient [:yellow :darkblue]))))
