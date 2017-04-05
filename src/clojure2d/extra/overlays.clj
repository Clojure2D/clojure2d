(ns clojure2d.extra.overlays
  (:require [clojure2d.core :as core]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r])
  (:import [java.awt.image BufferedImage]
           [java.awt Color]
           [clojure2d.pixels Pixels]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def add-compose (:add c/blends))

(defn- blend-shift-and-add-f
  ""
  [ch ^Pixels p1 ^Pixels p2 x y]
  (let [c1 (p/get-value p1 ch x y)
        c2 (p/get-value p2 ch (dec ^long x) y)]
    (c/convert-and-blend add-compose c1 c2)))

(defn draw-lines
  ""
  [canvas ^long w ^long h]
  (dorun 
   (for [y (range 0 h 3)]
     (let [y+ (inc ^long y)
           y++ (inc y+)]
       (core/set-color canvas (r/irand 180 200) 0 0 42)
       (core/line canvas 0 y w y)
       (core/set-color canvas 0 (r/irand 180 200) 0 42)
       (core/line canvas 0 y+ w y+)
       (core/set-color canvas 0 0 (r/irand 180 200) 42)
       (core/line canvas 0 y++ w y++))))
  canvas)

(defn render-rgb-scanlines
  ""
  [^BufferedImage p]
  (let [w (.getWidth p)
        h (.getHeight p)
        tinter1 (partial p/filter-channels (p/make-tint-filter 255 142 25))
        tinter2 (partial p/filter-channels (p/make-tint-filter 46 142 255))
        ^Pixels rimg (-> p
                         (core/resize-image (int (/ w 1.6)) (int (/ h 1.6)))
                         (core/resize-image w h)
                         (p/get-image-pixels))
        ^Pixels l1 (tinter1 rimg)
        ^Pixels l2 (tinter2 rimg)
        canvas (core/with-canvas (core/create-canvas w h)
                 (core/image (p/image-from-pixels l1))
                 (draw-lines w h))]
    
    (let [^Pixels l1 (p/get-canvas-pixels canvas)]
      (p/image-from-pixels (p/blend-channels (partial p/blend-channel-xy blend-shift-and-add-f) l1 l2)))))

;; noise canvas

(defn make-noise
  ""
  [alpha w h]
  (let [fc (fn [v] 
             (c/clamp255 (+ 100.0 (* 20.0 ^double (r/grand)))))
        fa (fn [v] alpha)
        ^Pixels p (p/filter-channels (partial p/filter-channel fc) nil nil (partial p/filter-channel fa) (p/make-pixels w h))]
    (p/set-channel p 1 (p/get-channel p 0))
    (p/set-channel p 2 (p/get-channel p 0))
    (p/image-from-pixels p)))

(defn render-noise
  ""
  ([noise ^BufferedImage img]
   (let [w (.getWidth img)
         h (.getHeight img)
         canvas (core/with-canvas (core/create-canvas w h)
                  (core/image img)
                  (core/image noise))]
     (@canvas 1)))
  ([^BufferedImage img]
   (render-noise (make-noise 80 (.getWidth img) (.getHeight img)) img)))

;; spots

(defn spots
  ""
  [^double alpha ^double basecolor ^long w ^long h]
  (let [size (* 4 w h)
        limita (int (min 5.0 (* 1.0e-5 (/ size 4.0))))
        limitb (int (min 6.0 (* 6.0e-5 (/ size 4.0))))
        ^ints pc (int-array size)
        ^ints pa (int-array size)
        alphas (/ alpha 255.0)]
    (dorun (repeatedly (r/irand limita limitb)
                       #(let [^int i (r/irand 10 (- w 10))
                              ^int j (r/irand 10 (- h 10))]
                          (dorun (for [^int m (range i (+ i ^int (r/irand 1 8)))
                                       ^int n (range (- j ^int (r/irand 6)) (+ j ^int (r/irand 1 6)))]
                                   (let [bc (-> ^double (r/grand)
                                                (* 40.0)
                                                (+ basecolor)
                                                (int))
                                         a (-> ^double (r/grand)
                                               (* 30.0)
                                               (+ 180.0)
                                               (m/constrain 0.0 255.0)
                                               (* alphas)
                                               (int))]
                                     (aset pc (+ m (* w n)) bc)
                                     (aset pa (+ m (* w n)) a)))))))
    (let [^Pixels p (p/make-pixels w h)]
      (p/set-channel p 0 pc)
      (p/set-channel p 3 pa)
      (let [res (p/filter-channels p/dilate-filter nil nil p/dilate-filter p)]
        (p/set-channel res 1 (p/get-channel res 0))
        (p/set-channel res 2 (p/get-channel res 1))
        (p/image-from-pixels res)))))

(defn make-spots
  ""
  [alpha vcolors w h]
  (doall (map #(spots alpha % w h) vcolors)))

(defn apply-images
  ""
  [canvas img spots]
  (core/image canvas img)
  (doseq [^BufferedImage s spots]
    (core/image canvas s))
  canvas)

(defn render-spots
  ""
  ([alpha vcolors ^BufferedImage img]
   (let [spots (make-spots alpha vcolors (.getWidth img) (.getHeight img))]
     (render-spots spots img)))
  ([spots ^BufferedImage img]
   (let [w (.getWidth img)
         h (.getHeight img)
         canvas (core/with-canvas (core/create-canvas w h)
                  (apply-images img spots))]     
     (@canvas 1))))
