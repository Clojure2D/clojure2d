;; http://www.brainfillingcurves.com/

(ns examples.ex37-fractal-bestiary
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [clojure2d.math.random :as r])
  (:import [clojure2d.math.vector Vec2]))

(def ^:const ^int size 800)
(def ^:const ^int hsize (/ size 2))


(def canvas (make-canvas size size :high))
(def window (show-window canvas "Fractal Bestiary"))



(defn draw-beast 
  ""
  [canvas ^long depth ^double len [a1 b1 a2 b2 a3 b3 a4 b4 :as all]]

;;;;;;;
  
  (rotate canvas (- m/HALF_PI))
  
  (push-matrix canvas)
  (when a1
    (translate canvas len 0)
    (flip-y canvas)
    (rotate canvas m/PI)) 
  (when b1 (flip-y canvas))  

  (if (zero? depth)
    (do
      (line canvas 0 0 len 0))
    (do
      (rotate canvas (- m/HALF_PI))
      (draw-beast canvas (dec depth) (* len (/ (m/sqrt 4.0))) all)))
  (pop-matrix canvas)

;;;;;;;;
  
  (translate canvas len 0)
  (rotate canvas m/HALF_PI)
  
  (push-matrix canvas)
  (when a2
    (translate canvas len 0)
    (flip-y canvas)
    (rotate canvas m/PI))
  (when b2 (flip-y canvas))  

  (if (zero? depth)
    (do
      (line canvas 0 0 len 0)) 
    (do
      (rotate canvas (- m/HALF_PI))
      (draw-beast canvas (dec depth) (* len (/ (m/sqrt 4.0))) all)))
  (pop-matrix canvas)


;;;;;

  (translate canvas len 0)
  (rotate canvas m/HALF_PI)
  
  (push-matrix canvas)
  (when a3
    (translate canvas len 0)
    (flip-y canvas)
    (rotate canvas m/PI))
  (when b3 (flip-y canvas))  

  (if (zero? depth)
    (do
      (line canvas 0 0 len 0)) 
    (do
      (rotate canvas (- m/HALF_PI))
      (draw-beast canvas (dec depth) (* len (/ (m/sqrt 4.0))) all)))
  (pop-matrix canvas)

;;;;;;;

  (translate canvas len 0)
  (rotate canvas (- m/HALF_PI))
  
  (push-matrix canvas)
  (when a4
    (translate canvas len 0)
    (flip-y canvas)
    (rotate canvas m/PI))
  (when b4 (flip-y canvas))  

  (if (zero? depth)
    (do
      (line canvas 0 0 len 0)) 
    (do
      (rotate canvas (- m/HALF_PI))
      (draw-beast canvas (dec depth) (* len (/ (m/sqrt 4.0))) all)))
  (pop-matrix canvas)

  
  canvas)

(let [v [true false false true true false false true]]
  (prn v)
  (with-canvas canvas
    (set-background 21 20 25)
    (set-color :lightgrey 250)
    (set-stroke 0.8)
    (translate 200 500)
    (draw-beast 1 150 (map not v))))
