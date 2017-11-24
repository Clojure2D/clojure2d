(ns examples.NOC.ch08.treestatic-8-6
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]))

(def canvas (make-canvas 800 200 :mid))
(def window (show-window canvas "Tree Static 8_6"))

(defn branch
  "Generate branches."
  [canvas len]
  (let [nlen (* 0.66 len)]

    (-> canvas
        (line 0 0 0 (- len))
        (translate 0 (- len)))

    (if (> nlen 2.0)
      (-> canvas
          (push-matrix)
          (rotate (/ m/PI 5.0))
          (branch nlen)
          (pop-matrix)
          (push-matrix)
          (rotate (/ m/PI -5.0))
          (branch nlen)
          (pop-matrix))
      canvas)))

(with-canvas-> canvas
  (set-background :white)
  (set-color :black)
  (set-stroke 2.0)
  (translate (/ (width canvas) 2) (height canvas))
  (branch 60))
