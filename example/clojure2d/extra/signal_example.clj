(ns clojure2d.extra.signal-example
  (:require [metadoc.examples :refer :all]
            [clojure2d.extra.signal :refer :all]
            [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [fastmath.vector :as v]
            [fastmath.core :as m]
            [fastmath.random :as r])
  (:import [clojure2d.extra.signal Signal]))

(r/set-seed! r/default-rng 42)

(add-examples signal
  (example "Create signal from sequence" (signal (repeatedly 1000 rand))))

(add-examples pixels->signal
  (example "Basic usage" (pixels->signal (p/load-pixels "docs/cockatoo.jpg")))
  (example "Custom configuration" (pixels->signal (p/load-pixels "docs/cockatoo.jpg") {:planar? false
                                                                                       :signed? true
                                                                                       :bits 24
                                                                                       :coding :ulaw
                                                                                       :channels [2 1]})))

(add-examples signal->pixels
  (example "Basic usage" (let [p (p/load-pixels "docs/cockatoo.jpg")
                               target (p/clone-pixels p)]
                           (signal->pixels (pixels->signal p) target)))
  (example "Custom configuration" (let [p (p/load-pixels "docs/cockatoo.jpg")
                                        target (p/clone-pixels p)
                                        conf-in {:planar? false
                                                 :signed? true
                                                 :bits 24
                                                 :coding :alaw
                                                 :channels [2 1]}
                                        conf-out (merge conf-in {:coding :alaw-rev})]
                                    (signal->pixels (pixels->signal p conf-in) target conf-out))))

(add-examples compose-effects
  (example "Usage" (let [lpf (effect :simple-lowpass)
                         hpf(effect :simple-highpass {:cutoff 2000.0})
                         composed (compose-effects lpf hpf)
                         result (composed 0.5)]
                     {"Composed effects" composed
                      "After call on sample" result
                      "Extract sample" (result)})))

(add-examples apply-effects
  (example "Usage" (let [lpf (effect :simple-lowpass {:cutoff 10000})
                         sgnal (signal [-1.0 1.0 -0.5 0.5 -0.1 0.1 0 0])]
                     (seq (apply-effects sgnal lpf))))
  (example "Reset state every two samples" (let [lpf (effect :simple-lowpass {:cutoff 10000})
                                                 sgnal (signal [-1.0 1.0 -0.5 0.5 -0.1 0.1 0 0])]
                                             (seq (apply-effects sgnal lpf 2)))))

(defsnippet clojure2d.extra.signal saver "Save pixels to image."
  (let [n (str "images/signal/" (first opts) ".jpg")]
    (save (f (p/load-pixels "docs/cockatoo.jpg")) (str "docs/" n))
    (str "../" n)))

(add-examples effects-filter
  (example-snippet "Apply lpf to the image" saver :image
    (fn [in]
      (let [lpf (effects-filter (effect :simple-highpass {:cutoff 1000}))]
        (p/filter-channels lpf in))))
  (example-snippet "Apply lpf to the image, with some conversion parametrization" saver :image
    (fn [in]
      (let [lpf (effects-filter (effect :simple-lowpass {:cutoff 500}) {:signed? true} {:signed? true} 15)]
        (p/filter-channels lpf in)))))

(add-examples apply-effects-to-pixels
  (example-snippet "Apply hpf to all channels" saver :image
    (fn [in]
      (apply-effects-to-pixels (effect :simple-highpass {:cutoff 10}) {:channels :all :bits 16} {:channels :all :planar? false :bits 16} in))))

(add-examples db->linear (example (db->linear 0.5)))
(add-examples linear->db (example (linear->db 0.5)))

(add-examples effect
  (example-session "Basic usage" (effect :fm) (effect :fm {:quant 5}))
  (example "Use as a function" (let [fm (effect :fm)]
                                 (fm 0.5)))
  (example "Extract last result." (let [fm (effect :fm)
                                        result (fm 0.5)]
                                    (result))))

(add-examples effects-list
  (example "List of all effects" effects-list))

(add-examples save-signal
  (example (let [s (pixels->signal (p/load-pixels "docs/cockatoo.jpg"))]
             (save-signal s "signal.raw"))))

(add-examples load-signal
  (example (let [s (pixels->signal (p/load-pixels "docs/cockatoo.jpg"))]
             (save-signal s "signal.raw")
             {:out (nth (load-signal "signal.raw") 11500)
              :in (nth s 11500)})))

(add-examples wave
  (example-session "Usage"
    (let [wave-sin (wave :sin 1.0 1.0 0.5)]
      (wave-sin 0.1))
    (let [wave-noise (wave :noise 0.1 1.0 0.123)]
      (wave-noise 0.1))))

(add-examples oscillators
  (example "List of oscillators" oscillators))

(add-examples sum-waves
  (example (let [w1 (wave :sin 1.0 0.8 1.0)
                 w2 (wave :noise 20.0 0.5 0.1)
                 sum (sum-waves w1 w2)]
             (sum 0.5))))

(add-examples wave->signal
  (example (let [w (wave :square 0.2 0.9 0.2)]
             (wave->signal w 44100 5))))

;;

(defn- plot-signals
  ""
  [& signals]
  (let [c (canvas 500 150)
        pal (c/palette-presets :dark2)]
    (with-canvas [c c]
      (set-background c (c/gray 0xf2))
      (set-stroke c 1.5)
      (doseq [[^Signal s col] (map vector signals pal)]
        (let [coords (map #(let [idx (int (m/norm % 0 (width c) 0 (count s)))
                                 y (m/norm (aget ^doubles (.signal s) idx) 1.2 -1.2 0 (height c))]
                             (v/vec2 % y)) (range (width c)))]
          (set-color c (c/darken col) 240)
          (path c coords))))
    c))

(defsnippet clojure2d.extra.signal draw-effect "Draw signal and applied effect"
  (let [n (str "images/signal/" (first opts) ".jpg")
        sig (wave->signal (sum-waves (wave :square 1.0 0.4 0.1)
                                     (wave :sin 5.0 0.4 0.1)
                                     (wave :saw 2.0 0.2 0.78)
                                     (wave :noise 32.0 0.8 0.1)) 200 5)
        sigout (apply-effects sig f)]
    (save (plot-signals sig sigout) (str "docs/" n))
    (str "../" n)))

(defsnippet clojure2d.extra.signal draw-effect-on-pixels "Image with applied effect"
  (let [n (str "images/signal/" (first opts) ".jpg")
        pix (apply-effects-to-pixels f (p/load-pixels "docs/cockatoo.jpg"))]
    (save pix (str "docs/" n))
    (str "../" n)))

(add-examples effect
  (example-snippet ":simple-lowpass" draw-effect :image (effect :simple-lowpass {:rate 200 :cutoff 2}))
  (example-snippet ":simple-highpass" draw-effect :image (effect :simple-highpass {:rate 200 :cutoff 10}))
  (example-snippet ":biquad-eq" draw-effect :image (effect :biquad-eq {:fs 200 :fc 10 :gain -10 :bw 4.0}))
  (example-snippet ":biquad-hs" draw-effect :image (effect :biquad-hs {:fs 200 :fc 2 :gain -10 :slope 2.0}))
  (example-snippet ":biquad-ls" draw-effect :image (effect :biquad-ls {:fs 200 :fc 5 :gain 5 :slope 2.0}))
  (example-snippet ":biquad-bp" draw-effect :image (effect :biquad-bp {:fs 200 :fc 15 :bw 2}))
  (example-snippet ":biquad-lp" draw-effect :image (effect :biquad-lp {:fs 200 :fc 15 :bw 2}))
  (example-snippet ":biquad-hp" draw-effect :image (effect :biquad-hp {:fs 200 :fc 15 :bw 2}))
  (example-snippet ":dj-eq" draw-effect :image (effect :dj-eq {:rate 200 :high -10 :low -20 :mid 5.0 :peak-bw 5 :shelf-slope 5}))
  (example-snippet ":phaser-allpass" draw-effect :image (effect :phaser-allpass {:delay 0.1}))
  (example-snippet ":divider" draw-effect :image (effect :divider {:denom 1}))
  (example-snippet ":fm" draw-effect :image (effect :fm {:quant 0.0 :omega 0.025 :phase 0.05}))
  (example-snippet ":bandwidth-limit" draw-effect :image (effect :bandwidth-limit {:rate 200 :freq 10}))
  (example-snippet ":distort" draw-effect :image (effect :distort {:factor 0.1}))
  (example-snippet ":foverdrive" draw-effect :image (effect :foverdrive {:drive 3.0}))
  (example-snippet ":decimator" draw-effect :image (effect :decimator {:bits 4 :rate 200 :fs 10}))
  (example-snippet ":basstreble" draw-effect :image (effect :basstreble {:rate 200 :treble-freq 60 :bass-freq 2.0 :bass -5 :treble -5 :gain 2.0 :slope 1.0}))
  (example-snippet ":echo" draw-effect :image (effect :echo {:delay 0.912 :decay 0.3 :rate 200}))
  (example-snippet ":vcf303" draw-effect :image (effect :vcf303 {:gain 3.0 :rate 200 :trigger false :cutoff 0.1 :resonance 1.1 :env-mod 0.9}))
  (example-snippet ":slew-limit" draw-effect :image (effect :slew-limit {:rate 200 :maxrise 2 :maxfall 10}))
  (example-snippet ":mda-thru-zero" draw-effect :image (effect :mda-thru-zero {:rate 200 :speed 0.9 :depth 1.0 :mix 0.8 :depth-mod 1.9 :feedback 0.9})))

(add-examples effect
  (example-snippet ":simple-lowpass" draw-effect-on-pixels :image (effect :simple-lowpass {:rate 200 :cutoff 2}))
  (example-snippet ":simple-highpass" draw-effect-on-pixels :image (effect :simple-highpass {:rate 200 :cutoff 10}))
  (example-snippet ":biquad-eq" draw-effect-on-pixels :image (effect :biquad-eq {:fs 200 :fc 10 :gain -10 :bw 4.0}))
  (example-snippet ":biquad-hs" draw-effect-on-pixels :image (effect :biquad-hs {:fs 200 :fc 2 :gain -10 :slope 2.0}))
  (example-snippet ":biquad-ls" draw-effect-on-pixels :image (effect :biquad-ls {:fs 200 :fc 5 :gain 5 :slope 2.0}))
  (example-snippet ":biquad-bp" draw-effect-on-pixels :image (effect :biquad-bp {:fs 200 :fc 15 :bw 2}))
  (example-snippet ":biquad-lp" draw-effect-on-pixels :image (effect :biquad-lp {:fs 200 :fc 15 :bw 2}))
  (example-snippet ":biquad-hp" draw-effect-on-pixels :image (effect :biquad-hp {:fs 200 :fc 15 :bw 2}))
  (example-snippet ":dj-eq" draw-effect-on-pixels :image (effect :dj-eq {:rate 200 :high -10 :low -20 :mid 5.0 :peak-bw 5 :shelf-slope 5}))
  (example-snippet ":phaser-allpass" draw-effect-on-pixels :image (effect :phaser-allpass {:delay 0.1}))
  (example-snippet ":divider" draw-effect-on-pixels :image (effect :divider {:denom 1}))
  (example-snippet ":fm" draw-effect-on-pixels :image (effect :fm {:quant 0.0 :omega 0.025 :phase 0.05}))
  (example-snippet ":bandwidth-limit" draw-effect-on-pixels :image (effect :bandwidth-limit {:rate 200 :freq 10}))
  (example-snippet ":distort" draw-effect-on-pixels :image (effect :distort {:factor 0.1}))
  (example-snippet ":foverdrive" draw-effect-on-pixels :image (effect :foverdrive {:drive 3.0}))
  (example-snippet ":decimator" draw-effect-on-pixels :image (effect :decimator {:bits 4 :rate 200 :fs 10}))
  (example-snippet ":basstreble" draw-effect-on-pixels :image (effect :basstreble {:rate 200 :treble-freq 60 :bass-freq 2.0 :bass -5 :treble -5 :gain 2.0 :slope 1.0}))
  (example-snippet ":echo" draw-effect-on-pixels :image (effect :echo {:delay 0.912 :decay 0.3 :rate 200}))
  (example-snippet ":vcf303" draw-effect-on-pixels :image (effect :vcf303 {:gain 3.0 :rate 200 :trigger false :cutoff 0.1 :resonance 1.1 :env-mod 0.9}))
  (example-snippet ":slew-limit" draw-effect-on-pixels :image (effect :slew-limit {:rate 200 :maxrise 2 :maxfall 10}))
  (example-snippet ":mda-thru-zero" draw-effect-on-pixels :image (effect :mda-thru-zero {:rate 200 :speed 0.9 :depth 1.0 :mix 0.8 :depth-mod 1.9 :feedback 0.9})))

(defsnippet clojure2d.extra.signal draw-wave "Draw wave"
  (let [n (str "images/signal/" (first opts) ".jpg")]
    (save (plot-signals (wave->signal f 200 5)) (str "docs/" n))
    (str "../" n)))

(defmacro wave-examples
  []
  `(add-examples wave ~@(for [osc oscillators]
                          `(example-snippet ~(name osc) draw-wave :image (wave ~osc 1.0 1.0 0.1)))))

(wave-examples)

(add-examples sum-waves
  (example-snippet "Sum waves plot" draw-wave :image (let [w1 (wave :sin 1.0 0.8 1.0)
                                                           w2 (wave :noise 20.0 0.5 0.1)]
                                                       (sum-waves w1 w2))))

