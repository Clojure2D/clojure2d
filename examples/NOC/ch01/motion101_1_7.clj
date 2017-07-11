(ns examples.NOC.ch01.motion101-1-7
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.vector :as v]
            [clojure2d.math.random :as r]
            [clojure2d.math :as m])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defprotocol MoverProto
  (update-mover [t w h])
  (display-mover [t canvas]))

(deftype Mover [^Vec2 position
                velocity]
  MoverProto
  (update-mover [_ w h]
    (let [^Vec2 nposition (v/add position velocity)]
      (Mover. (Vec2. (m/wrap 0 w (.x nposition))
                     (m/wrap 0 h (.y nposition)))
              velocity)))
  (display-mover [m canvas]
    (-> canvas
        (set-background 255 255 255)
        (set-stroke 2.0)
        (set-color 127 127 127)
        (ellipse (.x position) (.y position) 48 48)
        (set-color 0 0 0)
        (ellipse (.x position) (.y position) 48 48 true))
    m))

(defn make-mover
  "Create Mover"
  [w h]
  (Mover. (Vec2. (r/drand w) (r/drand h))
          (Vec2. (r/drand -2.0 2.0) (r/drand -2.0 2.0))))

;; run few times
(show-window (make-canvas 640 360) "Motion 101 (1.7)"
             (fn [canvas window _ mover]
               (let [m (or mover (make-mover (width window) (height window)))]
                 (display-mover (update-mover m (width window) (height window)) canvas))))
