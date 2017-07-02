;; apply analog filter on image

(ns examples.ex16-analog
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [clojure2d.math :as m]
            [clojure2d.extra.signal :refer :all])
  (:import [clojure2d.pixels Pixels]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; load image
(def ^Pixels p (p/load-pixels "results/test.jpg"))

(def canvas (make-canvas (.w p) (.h p)))
(def window (show-window canvas "Signal" (.w p) (.h p) 15))

(defmethod key-pressed ["Signal" \space] [_]
  (save-canvas canvas (next-filename "results/ex16/" ".jpg")))

;; dj-eq
(def effect1 (make-effect :dj-eq {:lo -10 :mid 10 :hi -10 :peak_bw 1.3 :shelf_slope 1.5 :rate 44100}))

;; lowpass
(def effect2 (make-effect :simple-lowpass {:rate 44100 :cutoff 1000}))

;; filter with dj-eq
(time (p/set-canvas-pixels! canvas (p/filter-channels p/normalize (p/filter-channels (make-effect-filter effect1 {:coding :alaw-rev} {:coding :ulaw}) p))))

;; filter with 3 lowpass
(time (p/set-canvas-pixels! canvas (p/filter-channels (make-effects-filter [effect2 effect2 effect2] {:signed true} {:signed true}) p)))
;; filter with all in YPbPr colorspace
(time (let [filter (make-effects-filter [effect1 effect2])
            res (->> p
                     (p/filter-colors c/to-YPbPr)
                     (p/filter-channels filter)
                     (p/filter-channels p/normalize)
                     (p/filter-channels p/equalize-filter false)
                     (p/filter-colors c/from-YPbPr))]
        (p/set-canvas-pixels! canvas res)))

(time (let [filter (make-effect-filter (make-effect :divider {:denominator 2}))
            res (->> p
                     (p/filter-colors c/to-OHTA)
                     (p/filter-channels p/normalize)
                     (p/filter-channels p/equalize-filter false)
                     (p/filter-channels filter filter filter nil)
                     (p/filter-channels p/normalize)
                     (p/filter-channels p/equalize-filter false)
                     (p/filter-colors c/from-YPbPr))]
        (p/set-canvas-pixels! canvas res)))

;; full process without use of filter-channels
(time (let [effect (make-effect :biquad-eq {:fc 2000 :gain -30 :bw 1 :fs 100000})
            in (signal-from-pixels p {:layout :interleaved
                                      :coding :alaw
                                      :signed true
                                      :channels [2 0 1]
                                      :bits 16})
            res (apply-effect effect in)
            resp (signal-to-pixels (p/clone-pixels p) res {:layout :interleaved
                                                           :coding :alaw-rev
                                                           :signed true
                                                           :channels [2 0 1]
                                                           :bits 16})]
        (p/set-canvas-pixels! canvas (p/filter-channels p/normalize resp))))

;; fm filter

(time (let [effect (make-effect-filter (make-effect :fm {:quant 10 :omega (* m/TWO_PI 0.00225857) :phase 0.00822}) (.w p))
            res (p/filter-channels effect nil p)]
        (p/set-canvas-pixels! canvas res)))

;; saving and loading signal to and from file
(save-signal (signal-from-pixels p {:layout :interleaved
                                    :coding :alaw
                                    :signed true
                                    :channels [2 0 1]
                                    :bits 16}) "results/ex16/signal.raw")

(load-signal "results/ex16/signal.raw")
