(ns NOC.ch02.attraction-2-6
  (:require [clojure2d.color :as c]
            [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v])
  (:import clojure2d.math.vector.Vec2))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int w 640)
(def ^:const ^int h 360)
(def ^:const ^double amass 20.0)

(defprotocol MoverProto
  (update-and-draw [t canvas window]))
(deftype Mover [^Vec2 position
                ^Vec2 velocity
                ^double mass]
  MoverProto
  (update-and-draw [_ canvas window]
    (let [att-pos (get-state window)
          force (v/sub att-pos position)
          d (m/constrain (v/mag force) 5.0 25.0)
          strength (/ (* amass mass) (m/sq d))
          nvelocity (v/add velocity (-> force
                                        v/normalize
                                        (v/mult strength)
                                        (v/div mass)))
          ^Vec2 nposition (v/add position nvelocity)]

      (-> canvas
          (set-color 127 127 127)
          (ellipse (.x nposition) (.y nposition) 16 16)
          (set-stroke 2.0)
          (set-color :black)
          (ellipse (.x nposition) (.y nposition) 16 16 true))
      
      (Mover. nposition nvelocity mass))))

(defn draw
  ""
  [canvas window framecount state]
  (let [mover (or state (Mover. (Vec2. 400 50) (Vec2. 1.0 0.0) 1.0))
        ^Vec2 pos (get-state window)]

    (-> canvas
        (set-background :white)
        (set-stroke 4.0)
        (set-color (if (< ^double (v/dist (mouse-pos window) pos) amass)
                     (c/make-color 100 100 100)
                     (c/make-color 175 175 175 200)))
        (ellipse (.x pos) (.y pos) (* 2.0 amass) (* 2.0 amass))
        (set-color :black)
        (ellipse (.x pos) (.y pos) (* 2.0 amass) (* 2.0 amass) true))

    (update-and-draw mover canvas window)))

(def window (show-window {:canvas (make-canvas w h)
                          :window-name "Attraction 2_6"
                          :draw-fn draw
                          :state (Vec2. (/ w 2) (/ h 2))}))

(defmethod mouse-event ["Attraction 2_6" :mouse-dragged] [e _]
  (mouse-pos e))

