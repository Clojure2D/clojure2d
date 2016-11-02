(ns clojure2d.extra.overlays
  (:require [clojure2d.core :as core]
            [clojure2d.pixels :as p]
            [clojure2d.utils :refer :all]
            [clojure2d.color :as c]
            [clojure2d.math :as m])
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
        c2 (p/get-value p2 ch (dec x) y)]
    (add-compose c1 c2)))

(defn draw-lines
  ""
  [canvas w h]
  (dorun 
     (for [y (range 0 h 3)]
       (let [y+ (inc y)
             y++ (inc y+)]
         (core/set-color canvas (Color. (int (m/irand 180 200)) 0 0 42))
         (core/line canvas 0 y w y)
         (core/set-color canvas (Color. 0 (int (m/irand 180 200)) 0 42))
         (core/line canvas 0 y+ w y+)
         (core/set-color canvas (Color. 0 0 (int  (m/irand 180 200)) 42))
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
    
    (let [^Pixels l1 (core/get-canvas-pixels canvas)]
      (p/image-from-pixels (p/blend-channels (partial p/blend-channel-xy blend-shift-and-add-f) l1 l2)))))

;; noise canvas

(defn make-noise
  ""
  [alpha w h]
  (let [fc (fn [v] 
            (int (c/clamp255 (+ 100 (* 20 (m/grand))))))
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
  [alpha basecolor w h]
  (let [size (* 4 w h)
        limita (min 5 (int (* 1.0e-5 (/ size 4))))
        limitb (min 6 (int (* 6.0e-5 (/ size 4))))
        ^ints pc (int-array size)
        ^ints pa (int-array size)
        alphas (/ alpha 255.0)]
    (dorun (repeatedly (m/irand limita limitb)
                       #(let [i (m/irand 10 (- w 10))
                              j (m/irand 10 (- h 10))]
                          (dorun (for [m (range i (+ i (m/irand 1 8)))
                                       n (range (- j (m/irand 6)) (+ j (m/irand 1 6)))]
                                   (let [bc (-> (m/grand)
                                                (* 40)
                                                (+ basecolor)
                                                (int))
                                         a (-> (m/grand)
                                               (* 30)
                                               (+ 180)
                                               (m/constrain 0 255)
                                               (* alphas)
                                               (int))]
                                     (aset pc ^int (+ m (* w n)) bc)
                                     (aset pa ^int (+ m (* w n)) a)))))))
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
