(ns generateme.folds
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v]
            [clojure2d.extra.variations :as vr]
            [clojure2d.extra.overlays :refer :all]
            [clojure.pprint :refer [pprint]])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:const ^double x1 -2.0)
(def ^:const ^double y1 -2.0)
(def ^:const ^double x2 2.0)
(def ^:const ^double y2 2.0)

(def ^:const ^double x1- (dec x1))
(def ^:const ^double y1- (dec y1))
(def ^:const ^double x2+ (inc x2))
(def ^:const ^double y2+ (inc y2))

(def options {:low [800 1.6 false]
              :medium [2048 3.4 true]
              :high [6000 3.4 true]})

(def config-atom (atom nil))

(def window (show-window (create-canvas 1 1) "folds" 800 800 10))

(defmethod key-pressed ["folds" \space] [_]
  (let [fn (next-filename "generateme/folds2d/" ".png")]
    (binding [*log-to-file* true]
      (log fn)
      (log (with-out-str (pprint @config-atom))))
    (save-canvas @(window 2) fn)))

(defn make-config
  "" 
  ([f-cfg]
   (let [config {:field-config f-cfg
                 :scale (if (r/brand 0.3) 0.7 (if (r/brand) 1.0 (r/drand 0.5 1.5)))
                 :shift (if (r/brand 0.7) [0.0 0.0] [(r/drand -1.0 1.0) (r/drand -1.0 1.0)])}]
     (reset! config-atom config)))
  ([] (make-config (vr/make-random-configuration))))

(defn make-me
  ""
  [canvas result-size {:keys [field-config ^double scale shift] :as cfg}]
  (let [[_ disp] window
        [shiftx shifty] shift
        ^Vec2 shiftv (Vec2. shiftx shifty)
        [^int width ^double step-size ellipse?] (result-size options)
        step (/ (- x2 x1) (* step-size width))
        field (vr/make-combination field-config)
        s60 (future (make-spots 60 [60 120 180] width width))
        n60 (future (make-noise 60 width width))] 

    (pprint cfg)
    
    (loop [y y1]
      (loop [x x1]
        
        (let [^Vec2 vv (->(Vec2. x y)
                          (v/add shiftv)
                          (field)
                          (v/mult scale)
                          (v/applyf m/sin)
                          (v/mult 2.7))
              xx (m/norm (+ (.x vv) ^double (r/grand 0.0012)) x1- x2+ 0.0 width)
              yy (m/norm (+ (.y vv) ^double (r/grand 0.0012)) y1- y2+ 0.0 width)]
          
          (if ellipse?
            (ellipse canvas xx yy 0.5 0.5)
            (point canvas xx yy)))

        (when (and @disp (< x x2)) (recur (+ x step))))
      (when (and @disp (< y y2)) (recur (+ y step))))

    (image canvas (render-noise @n60 (@canvas 1)))
    (image canvas (render-spots @s60 (@canvas 1))))

  (println (str result-size " DONE!\n---------------------\n"))
  
  :done)

(defn draw-folds
  ""
  ([result-size] (draw-folds result-size (make-config)))
  ([result-size config] 
   (let [width (first (result-size options))
         canvas (create-canvas width width)] 
     (future (with-canvas canvas
               (set-background 255 250 245)
               (set-color 35 35 35 16)
               (make-me result-size config)))
     (replace-canvas window canvas))))

(defmethod key-pressed ["folds" \m] [_]
  (draw-folds :medium (or @config-atom (make-config))))

(defmethod key-pressed ["folds" \l] [_]
  (draw-folds :low (or @config-atom (make-config))))

(defmethod key-pressed ["folds" \h] [_]
  (draw-folds :high (or @config-atom (make-config))))

(defmethod key-pressed ["folds" \n] [_]
  (draw-folds :low (make-config)))

(defmethod mouse-event ["folds" :mouse-clicked] [_]
  (draw-folds :low (make-config (vr/randomize-parametrization (:field-config @config-atom)))))

(draw-folds :low)


