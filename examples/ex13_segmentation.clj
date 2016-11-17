(ns examples.ex13-segmentations
  (:require [clojure2d.core :as core]
            [clojure2d.pixels :as p]
            [clojure2d.color :as clr]
            [clojure2d.extra.segmentation :as segm]
            [clojure2d.math :as m])
  (:import [clojure2d.pixels Pixels]
           [java.awt Color]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:const min-size 4) ; minimal block size
(def ^:const max-size 32) ; maximum block size
(def ^:const threshold 15) ; dividing threshold
(def ^:const channel 1) ; channel to operate with

(defn example-13
  "segment image based on selected channel, color segments and save"
  ([filename resfilename]
   (example-13 filename resfilename :default))
  ([filename resfilename strategy]
   (binding [p/*pixels-edge* 128] ; let's be sure we have some fixed value outside the image
       (let [^Pixels img (p/load-pixels filename)
             canvas (core/create-canvas (.w img) (.h img))
             segm (segm/segment-pixels-divide img channel min-size max-size threshold)
             iter (core/make-counter 0)

             draw (fn [canv] (doseq [[x y size] segm]
                               (let [defcol (Color. ^int (p/get-value img 0 x y)
                                                    ^int (p/get-value img 1 x y)
                                                    ^int (p/get-value img 2 x y))
                                     col (condp = strategy
                                           :bw (if (even? (iter))
                                                 Color/black
                                                 Color/white)
                                           :size (let [g (int (m/cnorm (m/logb 2 size) 0 6 5 255))]
                                                   (Color. g g g))
                                           defcol)]
                                 (core/set-color canv col)
                                 (core/rect canv x y size size))))]

         (core/with-canvas canvas
           (draw))

         (core/save-canvas canvas resfilename)))))

;; color with image colors
(example-13 "results/test.jpg" "results/ex13/colors.jpg")

;; color black and white
(example-13 "results/test.jpg" "results/ex13/bw.jpg" :bw)

;; color depends on size
(example-13 "results/test.jpg" "results/ex13/size.jpg" :size)
