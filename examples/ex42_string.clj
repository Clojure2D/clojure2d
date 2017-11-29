(ns examples.ex42-string
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [clojure2d.extra.variations :as vr]
            [clojure.pprint :refer [pprint]]
            [clojure2d.math.random :as r])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^long w 500)
(def ^:const ^long h 500)
(def ^:const ^long midh (/ h 2))

(def ^:const ^double x1 -2.5)
(def ^:const ^double y1 -2.5)
(def ^:const ^double x2 2.5)
(def ^:const ^double y2 2.5)
(def ^:const ^double step (/ (- x2 x1) 51))
(def ^:const ^double lines 15)
(def ^:const ^double lines- (dec 15))

(def ^:const ^double x1- (dec x1))
(def ^:const ^double y1- (dec y1))
(def ^:const ^double x2+ (inc x2))
(def ^:const ^double y2+ (inc y2))

(def ^:const ^double fscale 0.7)

(defn draw
  ""
  [canvas window ^long frame _]
  (let [field (get-state window)
        s (m/norm (max 0 ^int (mouse-x window)) 0 w 10 200)
        t (/ frame 100.0)]

    (-> canvas
        (set-background :black 50)
        (set-color :white 80))
    
    (dotimes [yl lines]
      (let [y (m/norm yl 0 lines- y1 y2)
            p (for [x (range x1 (+ x2 step) step) 
                    :let [nv (Vec2. (+ ^double x (- ^double (r/noise x y t) 0.5)) y)
                          cx (m/norm (m/cos (m/norm x x1 x2 (- m/PI) m/PI)) -1.0 1.0 0.0 s)
                          ^Vec2 vv (v/add nv (v/limit (v/mult (field nv) cx) 200))
                          xx (m/norm (+ ^double x (m/constrain (.x vv) -0.1 0.1)) x1- x2+ 0.0 w)]]
                (Vec2. xx (+ midh (.y vv))))]
        (path-bezier canvas p false true))))

  ;; (save canvas (next-filename "generateme/frames42/f" ".jpg"))

  )

(defn new-state
  "Create new combination."
  []
  (binding [vr/*skip-random-variations* true]
    (let [field-config (vr/make-random-configuration 3)]
      (pprint field-config)
      (vr/make-combination field-config))))

(close-session)
(def window (show-window {:canvas (make-canvas w h)
                          :width w
                          :height h
                          :window-name "String"
                          :state (new-state)
                          :draw-fn draw}))

(defmethod mouse-event [(:window-name window) :mouse-clicked] [_ _]
  (new-state))

(defmethod key-pressed [(:window-name window) \space] [_ s]
  (save window (next-filename "results/ex42/" ".jpg"))
  s)
