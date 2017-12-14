;; free_art_-_source 2017 exhibition in Galženica Gallery / Velika Gorica / Zagreb
;;
;; This code was created and modified live during "Silent Coding" presentation during free_art_-_source exhibition (on 25.03.2017 14:30-15:30)
;;
;; The main goal was to create source code from the scratch and build performance gradually. Two screens were presented: one with code and one with result
;; More info: http://www.formatc.hr/free_art_-_source
;; Photos from performance: http://subsite.hr/2017/03/performans-silent-coding-u-okviru-izlozbe-free_art_-_source/
;;
;; All comments and code cleaning were added after performance
;; Originally code was created without any order. This one should be run from top to bottom.
;;
;; When you run all the stuff (window is displayed and updater thread is running) you can change several things live:
;; * recreate palette variable
;; * change segmentation
;; * change sonification filter parameters, colorspaces, RAW settings
;; * in scenario: turn on/off all of the steps, change numerical parameters, compose method etc...
;;
;; All places which can be changed live are marked with ";;;; change!" comment

(ns examples-ex26-freeart-source
  (:require [clojure2d.color :as c]
            [clojure2d.core :refer :all]
            [clojure2d.extra.glitch :as g]
            [clojure2d.extra.overlays :as o]
            [clojure2d.extra.segmentation :as es]
            [clojure2d.extra.signal :as s]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.pixels :as p]))

;; turn on warnings
(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; frame width
(def ^:const w 640)

;; frame height
(def ^:const h 480)

;; number of frames
(def ^:const number-of-frames 44)

;; canvas bound to window
(def canvas (create-canvas w h))

;; canvases which represent frames
(def canvases (vec (repeatedly number-of-frames #(create-canvas w h))))

;; pixels from images (44 frames)
(def images (mapv #(p/load-pixels (str "examples/ex26/" (format "%02d" %) ".jpg")) (range number-of-frames)))

;; Iteratively go through canvases (frames) and draw them onto screen canvas
(defn draw
  "Draw current frame on the screen"
  [canvas window ^long frame state]
  (let [curr (or state canvases)
        id (mod frame number-of-frames)
        current-canvas (first curr)]
    (image canvas (get-image current-canvas))
    (next curr)))

;; Display window. I don't know why but running first time shows blank window.
;; Close and execute second time.
(def window (show-window canvas "free_art_-_source" (* w 2) (* h 2) 25 draw))

;; Prepare noise and spot overlay frames, it's slow
(def noise-frames (vec (repeatedly number-of-frames #(o/make-noise w h {:alpha 60})))) ;;;; change!
(def spots-frames (vec (repeatedly number-of-frames #(o/make-spots w h {:alpha 80 :intensities [30 220]})))) ;;;; change!

;; Sonification effect using DjEq filter with colorspace conversion.
;; Steps:
;; * create filter based on passed parameters
;; * convert pixels to desired colorspace
;; * convert pixels to RAW with defined settings
;; * apply filter
;; * convert RAW to pixels
;; * convert from colorspace
;; * normalize pixels and return
(defn sonification
  "Sonification based on two parameters"
  [^double t1 ^double t2 pixels]
  (let [effect (s/make-effect :dj-eq {:lo t1 :mid (- t2) :hi t2 :peak-bw 1.3 :shelf-slope 1.5 :rate 44100.0}) ;;;; change!
        cpx (p/filter-colors c/to-LUV pixels) ;;;; change!
        in (s/signal-from-pixels cpx {:layout :plane ;;;; change!
                                      :channels [0 1 2] ;;;; change!
                                      :bits 8 ;;;; change!
                                      :signed true ;;;; change!
                                      :coding :none}) ;;;; change!
        res (s/apply-effects effect in)
        resp (s/signal-to-pixels (p/clone-pixels pixels) res {:layout :plane ;;;; change!
                                                              :channels [0 1 2] ;;;; change!
                                                              :bits 8 ;;;; change!
                                                              :signed true ;;;; change!
                                                              :coding :none}) ;;;; change!
        respp (p/filter-colors c/from-HSB resp)] ;;;; change!
    (p/filter-channels p/normalize nil respp)))


;; prepare random palette for color reduction
(def palette (g/color-reducer-machine-random-config)) ;;;; change!

;; decompose images into segments
(binding [p/*pixels-edge* :wrap] ;;;; change!
  (def segments (mapv #(es/segment-pixels % 0 {:min-size 8 :max-size 128 :threshold 18.0}) images))) ;;;; change!

;; This code generates frame
;; Input:
;; * buffer canvas (note, this is not main canvas bound to screen)
;; * current time (number of frames passed from the beginning)
;; * current frame
;;
;; Final scenario is:
;; * draw black rectangle in the middle of the frame with some alpha, leaving 20px border
;; * draw noise based vertical lines as a simulation of old analog film
;; * draw ellipses (or rectangles) based on image segmentation. Not all segments are drawn (filter with some probability)
;; * operate on pixels:
;;   - blur
;;   - compose with images
;;   - sonificate with parameters changing during animation
;;   - reduce colors
;;   - equalize histogram
;; * apply overlays: rgb scanlines, noise and spots
(defn scenario
  "This is the function where all the things will happen"
  [canvas ^long time ^long frame]
  (let [color (mod time 255)] ;;;; change!
    (set-color canvas 0 0 0 200) ;;;; change!
    (rect canvas 20 20 (- w 40) (- h 40)) ;;;; change!

    (set-color canvas color (- 255 color) 11 200) ;;;; change!
    (dotimes [x w]
      (let [^double n (r/noise (/ frame 100.0) (/ frame 200.0) (/ x 20.0))] ;;;; change!
        (when (> n 0.636) ;;;; change!
          (line canvas x 0 x h))))

    (doseq [[x y size] (filter (fn [_] (r/brand 0.985)) (segments frame))] ;;;; change!
      (set-color canvas (p/get-color (images frame) x y))
      (ellipse canvas x y size size)) ;;;; change! (use rect)
    
    (->> (p/get-canvas-pixels canvas)
         (p/filter-channels p/gaussian-blur-1 nil) ;;;; change!
         (p/compose-channels :multiply (images frame)) ;;;; change!

         (sonification (* 5.0 (m/sin (/ time 45.0))) (* 5.0  (m/sin (/ time 50.0)))) ;;;; change!
         
                                        ; (g/color-reducer-machine palette) ;;;; change!
         (p/filter-channels p/equalize-filter nil) ;;;; change!
         (p/set-canvas-pixels! canvas))

    (with-canvas-> (canvases frame)
      (image (-> (get-image canvas)
                 (o/render-rgb-scanlines) ;;;; change!
                 (o/render-noise (noise-frames frame)) ;;;; change!
                 (o/render-spots (spots-frames frame))))))) ;;;; change!

;; atom controlling updater
(def is-running (atom true))

;; stop updater
(reset! is-running false)

;; enable updater
(reset! is-running true)

;; function which updates frames with scenario function
;; updater can be stopped by closing window or reseting @is-running to false
(defn updater
  "Update frames"
  []
  (let [buffer (create-canvas w h)]
    (loop [time (long 0)]
      (let [frame (mod time number-of-frames)]
        (with-canvas-> buffer
          (scenario time frame)))
      (when (and  @is-running (window-active? window)) (recur (inc time)))))
  (println "stopped"))

;; run updater in separate thread
(future (updater))

;; uncomment and run to save current state
(comment do
         (close-session)
         (dotimes [s number-of-frames]
           (save (canvases s) (next-filename "results/ex26/" ".jpg"))))
