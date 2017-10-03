(ns examples.ex39-old-school-tunnel
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.pixels :as p]
            [clojure2d.math.vector :as v])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^int w 300)
(def ^:const ^int h 300)

(def ^:const ^int hw (/ w 2))
(def ^:const ^int hh (/ h 2))

(def ^:const texture-type :generative) ;; change to :image

(def texture (p/load-pixels "results/test.jpg"))

(defn texture-image
  "Get color from image"
  [x y]
  (p/get-color texture x y))

(defn texture-gen
  "Generate texture"
  [^long x ^long y]
  (let [c1 (bit-and 0xff (bit-xor x y))
        c2 (bit-and 0xff (bit-xor (inc x) (inc y)))
        c3 (bit-and 0xff (bit-xor (dec x) (dec y)))]
    (v/vec4 c1 c2 c3 255)))

(def get-texture-color (if (= texture-type :generative) texture-gen texture-image))

(defn map-texture
  "Return texture color after transformation"
  [v ^Vec2 shift]
  (let [^double m (v/mag v)]
    (if (< m 0.04)
      :black
      (let [distance (+ (.x shift) (* 100.0 (/ 0.7 m)))
            angle (+ (.y shift) (* 100.0 (/ ^double (v/heading v) m/PI)))]
        (get-texture-color distance angle)))))

(defn draw
  "Draw frames"
  [canvas _ ^long fps _]
  (binding [p/*pixels-edge* :wrap]
    (let [t (/ fps 100.0)
          sa (* 0.5 (m/qcos (* 0.765 t)))
          ca (* 0.5 (m/qsin (+ t t)))
          shift (v/vec2 (* 5.0 fps) (/ fps 10.0))]
      (dotimes [x hw]
        (let [x2 (+ x x)
              ^double xx (m/norm x2 0.0 w -1.0 1.0)]
          (dotimes [y hh]
            (let [y2 (+ y y)
                  ^double yy (m/norm y2 0.0 h -1.0 1.0)]
              (set-color canvas (map-texture (v/vec2 (+ xx sa) (+ yy ca)) shift))
              (rect canvas x2 y2 2 2))))))))

(def canvas (make-canvas w h :low))
(def window (show-window canvas "Oldschool tunnel" 600 600 60 #(draw %1 %2 %3 %4)))

(defmethod key-pressed [(:window-name window) \space] [_ _]
  (save canvas (next-filename "results/ex39/" ".jpg")))
