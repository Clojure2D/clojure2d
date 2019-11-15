(ns clojure2d.extra.signal-example
  (:require [metadoc.examples :refer :all]
            [clojure2d.extra.signal :refer :all]
            [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [fastmath.vector :as v]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.signal :as s]))

(r/set-seed! r/default-rng 42)

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

(defsnippet clojure2d.extra.signal saver "Save pixels to image."
  (let [n (str "images/signal/" (first opts) ".jpg")]
    (save (f (p/load-pixels "docs/cockatoo.jpg")) (str "docs/" n))
    (str "../" n)))

(add-examples effects-filter
  (example-snippet "Apply lpf to the image" saver :image
    (fn [in]
      (let [lpf (effects-filter (s/effect :simple-highpass {:cutoff 1000}))]
        (p/filter-channels lpf in))))
  (example-snippet "Apply lpf to the image, with some conversion parametrization" saver :image
    (fn [in]
      (let [lpf (effects-filter (s/effect :simple-lowpass {:cutoff 500}) {:signed? true} {:signed? true} 15)]
        (p/filter-channels lpf in)))))

(add-examples apply-effects-to-pixels
  (example-snippet "Apply hpf to all channels" saver :image
    (fn [in]
      (apply-effects-to-pixels (s/effect :simple-highpass {:cutoff 10}) {:channels :all :bits 16} {:channels :all :planar? false :bits 16} in))))


(defsnippet clojure2d.extra.signal draw-effect-on-pixels "Image with applied effect"
  (let [n (str "images/signal/" (first opts) ".jpg")
        pix (apply-effects-to-pixels f (p/load-pixels "docs/cockatoo.jpg"))]
    (save pix (str "docs/" n))
    (str "../" n)))

(add-examples apply-effects-to-pixels
  (example-snippet ":simple-lowpass" draw-effect-on-pixels :image (s/effect :simple-lowpass {:rate 200 :cutoff 2}))
  (example-snippet ":simple-highpass" draw-effect-on-pixels :image (s/effect :simple-highpass {:rate 200 :cutoff 10}))
  (example-snippet ":biquad-eq" draw-effect-on-pixels :image (s/effect :biquad-eq {:fs 200 :fc 10 :gain -10 :bw 4.0}))
  (example-snippet ":biquad-hs" draw-effect-on-pixels :image (s/effect :biquad-hs {:fs 200 :fc 2 :gain -10 :slope 2.0}))
  (example-snippet ":biquad-ls" draw-effect-on-pixels :image (s/effect :biquad-ls {:fs 200 :fc 5 :gain 5 :slope 2.0}))
  (example-snippet ":biquad-bp" draw-effect-on-pixels :image (s/effect :biquad-bp {:fs 200 :fc 15 :bw 2}))
  (example-snippet ":biquad-lp" draw-effect-on-pixels :image (s/effect :biquad-lp {:fs 200 :fc 15 :bw 2}))
  (example-snippet ":biquad-hp" draw-effect-on-pixels :image (s/effect :biquad-hp {:fs 200 :fc 15 :bw 2}))
  (example-snippet ":dj-eq" draw-effect-on-pixels :image (s/effect :dj-eq {:rate 200 :high -10 :low -20 :mid 5.0 :peak-bw 5 :shelf-slope 5}))
  (example-snippet ":phaser-allpass" draw-effect-on-pixels :image (s/effect :phaser-allpass {:delay 0.1}))
  (example-snippet ":divider" draw-effect-on-pixels :image (s/effect :divider {:denom 1}))
  (example-snippet ":fm" draw-effect-on-pixels :image (s/effect :fm {:quant 0.0 :omega 0.025 :phase 0.05}))
  (example-snippet ":bandwidth-limit" draw-effect-on-pixels :image (s/effect :bandwidth-limit {:rate 200 :freq 10}))
  (example-snippet ":distort" draw-effect-on-pixels :image (s/effect :distort {:factor 0.1}))
  (example-snippet ":foverdrive" draw-effect-on-pixels :image (s/effect :foverdrive {:drive 3.0}))
  (example-snippet ":decimator" draw-effect-on-pixels :image (s/effect :decimator {:bits 4 :rate 200 :fs 10}))
  (example-snippet ":basstreble" draw-effect-on-pixels :image (s/effect :basstreble {:rate 200 :treble-freq 60 :bass-freq 2.0 :bass -5 :treble -5 :gain 2.0 :slope 1.0}))
  (example-snippet ":echo" draw-effect-on-pixels :image (s/effect :echo {:delay 0.912 :decay 0.3 :rate 200}))
  (example-snippet ":vcf303" draw-effect-on-pixels :image (s/effect :vcf303 {:gain 3.0 :rate 200 :trigger false :cutoff 0.1 :resonance 1.1 :env-mod 0.9}))
  (example-snippet ":slew-limit" draw-effect-on-pixels :image (s/effect :slew-limit {:rate 200 :maxrise 2 :maxfall 10}))
  (example-snippet ":mda-thru-zero" draw-effect-on-pixels :image (s/effect :mda-thru-zero {:rate 200 :speed 0.9 :depth 1.0 :mix 0.8 :depth-mod 1.9 :feedback 0.9})))
