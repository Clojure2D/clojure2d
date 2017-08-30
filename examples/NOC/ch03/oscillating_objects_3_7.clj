(ns NOC.ch03.oscillating-objects-3-7
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [clojure2d.math.random :as r])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int w 640)
(def ^:const ^int h 360)

(defprotocol OscillatorProto
  (oscillate [o])
  (display [o canvas]))

(deftype Oscillator [^Vec2 angle ^Vec2 velocity ^Vec2 amplitude]
  OscillatorProto
  (oscillate [_]
    (Oscillator. (v/add angle velocity) velocity amplitude))
  (display [o canvas]
    (let [^Vec2 xy (-> angle
                       (v/applyf m/sin)
                       (v/emult amplitude))]
      (-> canvas
          (set-stroke 2.0)
          (push-matrix) 
          (translate (/ w 2) (/ h 2))
          (set-color :black)
          (set-color 127 127 127 127)
          (ellipse (.x xy) (.y xy) 32 32)
          (set-color :black)
          (line 0 0 (.x xy) (.y xy))
          (ellipse (.x xy) (.y xy) 32 32 true)
          (pop-matrix)))
    
    o))

(defn make-oscillator
  "Create random oscillator"
  []
  (Oscillator. (Vec2. 0.0 0.0)
               (v/generate-vec2 #(r/drand -0.05 0.05))
               (Vec2. (r/drand 20 (/ w 2)) (r/drand 20 (/ h 2)))))

(defn draw
  ""
  [canvas window _ state]
  (let [movers (or state (repeatedly 10 #(make-oscillator)))]
    
    (set-background canvas :white)

    (mapv #(display (oscillate %) canvas) movers)))

(def window (show-window (make-canvas w h) "Oscillating objects 3_7" draw))
