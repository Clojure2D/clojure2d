;; apply analog filter on image

(ns examples.ex16-analog
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [clojure2d.math :as m]
            [clojure2d.extra.signal :refer :all])
  (:import [clojure2d.pixels Pixels]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; load image
(def ^Pixels p (p/load-pixels "results/test.jpg"))

(def canvas (make-canvas (width p) (height p)))
(def window (show-window canvas "Signal" 15 nil))

(defmethod key-pressed ["Signal" \space] [_ _]
  (save-canvas canvas (next-filename "results/ex16/" ".jpg")))

;; dj-eq
(def effect1 (make-effect :dj-eq {:lo -10 :mid 10 :hi -10 :peak-bw 1.3 :shelf-slope 1.5 :rate 44100}))

;; lowpass
(def effect2 (make-effect :simple-lowpass {:rate 44100 :cutoff 1000}))

;; 3 x lowpass
(def effect3 (compose-effects effect2 effect2 effect2))

;; filter with dj-eq
(time (p/set-canvas-pixels! canvas (p/filter-channels p/normalize (p/filter-channels (make-effects-filter effect1 {:coding :alaw-rev} {:coding :ulaw}) p))))

;; filter with 3 lowpass
(time (p/set-canvas-pixels! canvas (p/filter-channels (make-effects-filter effect3 {:signed true} {:signed true}) p)))

;; filter with all in YPbPr colorspace
(time (let [filter (make-effects-filter (compose-effects effect1 effect2))
            res (->> p
                     (p/filter-colors c/to-YPbPr)
                     (p/filter-channels filter)
                     (p/filter-channels p/normalize)
                     (p/filter-channels p/equalize-filter false)
                     (p/filter-colors c/from-YPbPr))]
        (p/set-canvas-pixels! canvas res)))

(time (let [filter (make-effects-filter (make-effect :divider {:denominator 2}))
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
            resp (apply-effects-to-pixels effect
                                          {:layout :interleaved
                                           :coding :alaw
                                           :signed true
                                           :channels [2 0 1]
                                           :bits 16}
                                          {:layout :interleaved
                                           :coding :alaw-rev
                                           :signed true
                                           :channels [2 0 1]
                                           :bits 16} p)]
        (p/set-canvas-pixels! canvas (p/filter-channels p/normalize resp))))

;; fm filter
(time (let [effect (make-effects-filter (make-effect :fm {:quant 10 :omega (* m/TWO_PI 0.00225857) :phase 0.00822}) (width p))
            res (p/filter-channels effect nil p)]
        (p/set-canvas-pixels! canvas res)))

;; Echo uses array of doubles internally to keep the state, don't use one effect in multiple threads. Filter-channels runs the same effect (with one state) in 3 threads for red, green and blue channels separately.
;; Wrong way (run multiple times)
(time (let [effect (make-effects-filter (make-effect :echo {}))
            res (p/filter-channels effect nil p)]
        (p/set-canvas-pixels! canvas res)))

;; Good way
(time (let [effect-r (make-effects-filter (make-effect :echo {}))
            effect-g (make-effects-filter (make-effect :echo {}))
            effect-b (make-effects-filter (make-effect :echo {}))
            res (p/filter-channels effect-r effect-g effect-b nil p)]
        (p/set-canvas-pixels! canvas res)))

;; vcf303 normalize (or equalize) after filtering
(time (let [effect (make-effects-filter (make-effect :vcf303 {:trigger true}) (* 50 ^long (width p)))
            res (p/filter-channels p/equalize-filter nil (p/filter-channels effect nil p))]
        (p/set-canvas-pixels! canvas res)))

;; vcf303 normalize (or equalize) after filtering
(time (let [effect (make-effect :slew-limit {:maxrise 50 :maxfall 1000})
            res (p/filter-channels p/normalize-filter nil (apply-effects-to-pixels effect {:signed true} {:signed true} p))]
        (p/set-canvas-pixels! canvas res)))

(time (let [effect (make-effect :mda-thru-zero {:speed 0.41 :depth 0.4 :feedback 0.2 :depth-mod 0.1 :mix 0.5})
            res (p/filter-channels p/normalize-filter nil (apply-effects-to-pixels effect {:signed true} {:signed true} p))]
        (p/set-canvas-pixels! canvas res)))

;; saving and loading signal to and from file
(save-signal (signal-from-pixels p {:layout :interleaved
                                    :coding :alaw
                                    :signed true
                                    :channels [2 0 1]
                                    :bits 16}) "results/ex16/signal.raw")

(load-signal "results/ex16/signal.raw")
