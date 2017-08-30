(ns examples.NOC.ch04.particlesystemsmoke-4-8
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.pixels :as p]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int w 640)
(def ^:const ^int h 360)

;; change to true for ellipse version (b)
(def ^:const version-b false)

(def img (p/load-pixels "examples/NOC/ch04/texture.png"))
(def filtered-images (mapv #(-> (p/make-tint-filter 255 255 255 (* ^double % 2.5))
                                (p/filter-channels img)
                                (p/image-from-pixels)) (range 51)))

(def ^:const img-hw (* ^int (width img) 0.5))
(def ^:const img-hh (* ^int (height img) 0.5))

(defprotocol ParticleProto
  (run [p canvas wind]))

(deftype Particle [pos vel ^int lifespan]
  ParticleProto
  (run [_ canvas wind]
    (let [nlifespan (dec lifespan)
          nvel (v/add vel wind)
          ^Vec2 npos (v/add pos nvel)]

      (if version-b
        (-> canvas
            (set-color :white (max 0 nlifespan))
            (ellipse (.x npos) (.y npos) (width img) (height img)))
        (image canvas (filtered-images (max 0 nlifespan)) (- (.x npos) img-hw) (- (.y npos) img-hh)))
      
      (Particle. npos nvel nlifespan))))

(defn make-particle
  "Create random Particle"
  []
  (Particle. (Vec2. (* w 0.5) (- h 75))
             (Vec2. (r/grand 0.3) (dec (r/grand 0.3)))
             50))

(defn draw
  "Draw arrow and smoke"
  [canvas window _ state]
  (let [particles (or state [(make-particle)])
        dx (m/norm (mouse-x window) 0 w -0.2 0.2)
        wind (Vec2. dx 0.0)
        len (* 500.0 ^double (v/mag wind))]

    (-> canvas ;; arrow
        (set-background :black)
        (set-color :white)
        (push-matrix)
        (translate (* w 0.5) 50)
        (rotate (v/heading wind))
        (line 0 0 len 0)
        (line len 0 (- len 4) 2)
        (line len 0 (- len 4) -2)
        (pop-matrix))
    
    (mapv #(run % canvas wind) (filter #(pos? (.lifespan ^Particle %)) (conj particles (make-particle) (make-particle))))))

(def window (show-window (make-canvas w h) "Particle system smoke 4_8" draw))
