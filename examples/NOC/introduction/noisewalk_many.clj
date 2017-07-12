(ns examples.NOC.introduction.noisewalk-many
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v]
            [clojure2d.math :as m]
            [clojure2d.color :as c])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; no deftype approach

(def nstep (Vec2. 0.01 0.01))
(def noise-seed (r/irand))

(defn draw
  ""
  [canvas window ^long framecount state]
  (let [walkers (or state (repeatedly 10 #(vector (Vec2. 0 0)
                                                  (v/generate-vec2 (partial r/drand 1000)))))
        octaves (int (m/cnorm (mouse-x window) 0 (width window) 1 8))
        noise (r/make-perlin-noise noise-seed octaves)
        total (m/constrain (/ framecount 30) 1 10)
        result (map (fn [[_ ^Vec2 noff]] 
                      (let [nx (m/norm (noise (.x noff)) 0.0 1.0 0 (width canvas))
                            ny (m/norm (noise (.y noff)) 0.0 1.0 0 (height canvas))]
                        [(Vec2. nx ny) (v/add noff nstep)])) walkers)]

    (set-background canvas c/:white)

    (dorun (take total (map #(let [[^Vec2 position] %] 
                               (-> canvas
                                   (set-color c/:gray)
                                   (ellipse (.x position) (.y position) 48 48)
                                   (set-stroke 2.0)
                                   (set-color c/:black)
                                   (ellipse (.x position) (.y position) 48 48 true))) result)))

    result))

(def window (show-window (make-canvas 600 400) "Noise walk - many" draw))
