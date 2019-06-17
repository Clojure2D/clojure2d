;; # Namespace scope
;;
;; Three overlays to give more analog view of the images.
;;
;; * `render-rgb-scanlines` - adds rgb tv scan lines slightly bluring image
;; * `render-crt-scanlines` - adds old monitor scan lines
;; * `make-noise` and `render-noise` - create and add white noise layer
;; * `make-spots` and `render-spots` - create and add small spots
;;
;; All functions operate on image type.
;;
;; See example 12 for usage

(ns clojure2d.extra.overlays
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [fastmath.core :as m]
            [fastmath.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; ## RGB scanlines

(def ^:private add-compose (:add c/blends))

(defn- blend-shift-and-add-f
  "Slightly shift channels"
  [ch p1 p2 x y]
  (let [c1 (p/get-value p1 ch x y)
        c2 (p/get-value p2 ch (dec ^long x) y)]
    (c/blend-values add-compose c1 c2)))

(defn- draw-lines
  "Draw rgb lines"
  [canvas ^long w ^long h]
  (dorun 
   (for [y (range 0 h 3)]
     (let [y+ (inc ^long y)
           y++ (inc y+)]
       (set-color canvas (r/irand 180 200) 0 0 42)
       (line canvas 0 y w y)
       (set-color canvas 0 (r/irand 180 200) 0 42)
       (line canvas 0 y+ w y+)
       (set-color canvas 0 0 (r/irand 180 200) 42)
       (line canvas 0 y++ w y++))))
  canvas)

(def ^:private tinter1 (partial p/filter-channels (p/tint (c/color 245 130 16))))
(def ^:private tinter2 (partial p/filter-channels (p/tint (c/color 36 130 225))))

(defn render-rgb-scanlines
  "Blurs and renders rgb stripes on the image, returns new image.

  Scale parameter (default 1.6) controls amount of blur. Resulting image is sligtly lighter and desaturated.

  Correct with normalize filter if necessary."
  ([p {:keys [^double scale] :or {scale 1.6}}] 
   (let [p (get-image p)
         ^int w (width p)
         ^int h (height p)
         rimg (-> p
                  (resize (unchecked-int (/ w scale)) (unchecked-int (/ h scale)))
                  (resize w h)
                  (p/to-pixels))
         l1 (tinter1 rimg)
         l2 (tinter2 rimg)
         cnvs (with-canvas-> (canvas w h)
                (image (get-image l1))
                (draw-lines w h))
         l1 (p/to-pixels cnvs)]
     (get-image (p/blend-channels (partial p/blend-channel-xy blend-shift-and-add-f) l1 l2))))
  ([p] (render-rgb-scanlines p {})))

;; ## CRT Scanlines

(defn- adjust-pos-value 
  "Adjust given position according to offset and resolution"
  [^double value ^double offset ^double resolution]
  (* resolution (m/floor (+ offset (/ value resolution)))))

(defn render-crt-scanlines
  "Create CRT scanlines and rgb patterns. Parameters:

  * resolution - size of the scanlines (default 6.0)
  * hardpix - horizontal blur, -2.0 soft, -8.0 hard (default -4.0)
  * hardscan - scanline softness, -4.0 soft, -16.0 hard (default -12.0)
  * mask-dark - crt mask dark part multiplier 0.25-1.0 (default 1.0, none)
  * mask-light - crt mask color part multiplier 1.0-1.75 (default 1.0, none)
  * mask-mult - crt mask pattern shift, 0.0+ (default 3.0, crt grid)

  Based on https://www.shadertoy.com/view/XsjSzR"
  ([img] (render-crt-scanlines img {}))
  ([img {:keys [^double resolution ^double hardpix ^double hardscan ^double mask-dark ^double mask-light ^int mask-mult]
         :or {resolution 6.0 hardpix -4.0 hardscan -12.0 mask-dark 1.0 mask-light 1.0 mask-mult 3.0}}]
   (let [img (get-image img)
         p (p/to-pixels img)]

     (letfn [(dist [^double pos]
               (let [poss (/ pos resolution)]
                 (- 0.5 (- poss (m/floor poss)))))
             (gauss [^double pos ^double scale]
               (m/pow 2.0 (* scale (m/sq pos))))
             (h3-blur [ch p ^long xx ^long yy]
               (let [y (adjust-pos-value yy 0.0 resolution)
                     x- (adjust-pos-value xx -1.0 resolution)
                     x (adjust-pos-value xx 0.0 resolution)
                     x+ (adjust-pos-value xx 1.0 resolution)
                     ^int a (p/get-value p ch x- y)
                     ^int b (p/get-value p ch x y)
                     ^int c (p/get-value p ch x+ y)
                     ^double dst (dist xx)
                     ^double wa (gauss (dec dst) hardpix)
                     ^double wb (gauss dst hardpix)
                     ^double wc (gauss (inc dst) hardpix)]
                 (-> (+ (* a wa) (+ (* b wb) (* c wc)))
                     (/ (+ wa wb wc))
                     (/ 255.0)
                     (c/to-linear)
                     (* 1000000.0))))]
       
       (let [blurred (p/filter-channels (partial p/filter-channel-xy h3-blur) nil p)
             tri (fn [^long ch p ^long xx ^long yy]
                   (let [y- (adjust-pos-value yy -1.0 resolution)
                         y (adjust-pos-value yy 0.0 resolution)
                         y+ (adjust-pos-value yy 1.0 resolution)
                         ^int a (p/get-value p ch xx y-)
                         ^int b (p/get-value p ch xx y)
                         ^int c (p/get-value p ch xx y+)
                         ^double dst (dist yy)
                         ^double wa (gauss (dec dst) hardscan)
                         ^double wb (gauss dst hardscan)
                         ^double wc (gauss (inc dst) hardscan)
                         xf (>>> (rem (+ xx (* mask-mult yy)) 6) 1)]
                     (-> (+ (* a wa) (+ (* b wb) (* c wc)))
                         (/ 1000000.0)
                         (c/from-linear) 
                         (* 255.0)
                         (* (if (== ch xf) mask-light mask-dark))
                         (m/constrain 0.0 255.0))))]
         (->> blurred
              (p/filter-channels (partial p/filter-channel-xy tri) nil)
              (get-image)))))))

;; ## Noise
;;
;; To apply noise overlay you have to perform two steps: first one is creating overlay with `make-noise` and then apply on image with `render-noise`. This way you can reuse overlay several times.

(defn noise-overlay
  "Create transparent image with noise (gaussian).

  `:alpha` parameter describes transparency (default: 80).

  Use with [[render-noise]]."
  ([w h {:keys [alpha] :or {alpha 80}}]
   (let [fc (fn [_] 
              (c/lclamp255 (+ 100.0 (* 20.0 (r/grand)))))
         fa (constantly (int alpha))
         p (p/filter-channels (partial p/filter-channel fc) nil nil (partial p/filter-channel fa) (p/pixels w h))]
     (p/set-channel p 1 (p/get-channel p 0))
     (p/set-channel p 2 (p/get-channel p 0))
     (get-image p)))
  ([w h] (noise-overlay w h {})))

(defn render-noise
  "Render noise on image."
  ([img noise-ovrly]
   (let [img (get-image img)
         w (width img)
         h (height img)
         cnvs (with-canvas-> (canvas w h)
                (image img)
                (image noise-ovrly))]
     (get-image cnvs)))
  ([img]
   (render-noise img (noise-overlay (width img) (height img)))))

;; ## Spots
;;
;; Similar to noise. First you have to create spots with `make-spots` and then you can apply them to the image. `make-spots` creates list of ovelays with provided intensities.

(defn- spots
  "Create transparent image with spots with set alpha and intensity"
  [^double alpha ^double intensity ^long w ^long h]
  (let [size (* 4 w h)
        limitx (int (m/constrain (/ w 200) 1 6))
        limity (int (m/constrain (/ h 200) 1 6))
        ^ints pc (int-array size)
        ^ints pa (int-array size)
        alphas (/ alpha 255.0)]
    (dorun (repeatedly (r/irand 1 (+ limitx limity))
                       #(let [i (r/irand 10 (- w 10))
                              j (r/irand 10 (- h 10))]
                          (dorun (for [m (range i (+ i (r/irand 2 8)))
                                       n (range (- j (r/irand 8)) (+ j (r/irand 1 8)))]
                                   (let [bc (-> (r/grand)
                                                (* 40.0)
                                                (+ intensity)
                                                (m/constrain 0.0 255.0)
                                                (int))
                                         a (-> (r/grand)
                                               (* 30.0)
                                               (+ 180.0)                                               
                                               (* alphas)
                                               (m/constrain 0.0 255.0)
                                               (int))]
                                     (aset pc (+ ^long m (* w ^long n)) bc)
                                     (aset pa (+ ^long m (* w ^long n)) a)))))))
    (let [p (p/pixels w h)]
      (p/set-channel p 0 pc)
      (p/set-channel p 3 pa)
      (let [res (p/filter-channels p/dilate nil nil p/dilate p)]
        (p/set-channel res 1 (p/get-channel res 0))
        (p/set-channel res 2 (p/get-channel res 0))
        (get-image res)))))

(defn spots-overlay
  "Create vector of spotted overlays. Input: spots transparency (default 80), list of intensities (int values from 0 to 255, default [60 120]) and size of overlay."
  ([w h {:keys [alpha intensities] :or {alpha 80 intensities [60 120]}}]
   (mapv #(spots alpha % w h) intensities))
  ([w h] (spots-overlay w h {})))

(defn- apply-images
  "Add all spotted overlays to image."
  [canvas img spots]
  (image canvas img)
  (doseq [s spots]
    (image canvas s))
  canvas)

(defn render-spots
  "Render spots on image. Returns image."
  ([img spots]
   (let [img (get-image img)
         w (width img)
         h (height img)
         cnvs (with-canvas-> (canvas w h)
                (apply-images img spots))]     
     (get-image cnvs)))
  ([img] (render-spots img (spots-overlay (width img) (height img)))))

