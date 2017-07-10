(ns examples.ex23-raymarching2
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :refer :all]
            [clojure2d.math.vector :as v]
            [clojure2d.color :as c]
            [clojure2d.extra.raymarching :as r])
  (:import [clojure2d.math.vector Vec2 Vec3]
           [clojure2d.extra.raymarching HitData Material]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:const ^long w 1200)
(def ^:const ^long h 1200)

(def canvas (create-canvas w h))

(def window (show-window canvas "raymarching2" 15 nil))

(defmethod key-pressed ["raymarching2" \space] [_]
  (save-canvas canvas "results/ex23/scene.jpg"))

(do
  (def ^:const ^double max-depth 20.0)

  ;; ray origin
  (def ro (Vec3. 0.0 4.5 4.0))
  (def nro (v/normalize (v/sub ro)))

  ;; camera
  (def camera (r/make-camera ro (Vec3. 0.0 2.0 2.0) 0.0))

  ;; materials
  (def m1 (Material. (v/div (Vec3. 3 101 100) 255.0) 0.5 0.1 0.99 555.0 0.0))
  (def m2 (Material. (v/div (Vec3. 13 23 52) 255.0) 0.5 0.1 0.99 555.0 0.2))
  (def m3 (Material. (v/div (Vec3. 255 0 0) 255.0) 0.5 0.1 0.99 555.0 0.0))
  (def ^Material bg (Material. (v/div (Vec3. 232 221 203) 255.0) 0.0 0.0 0.0 0.0 0.0))

  ;; object
  (def sphere (r/make-primitive :sphere m3 {:r 3.0}))
  (def box (r/make-primitive :box m2 {:box (Vec3. 2.5 2.5 2.5)}))

  ;; custom plane
  (defn fn-plane
    ""
    [^Vec3 p]
    (let [v (- (* 1.5 ^double (noise (* 0.06 (.x p)) (* 0.06 (.z p)))) 1.5)]
      (HitData. (- (.y p) v) m1)))

  ;; scene
  (def scene (r/op-blend fn-plane 
                         (r/op-rotate (r/op-subtract box sphere) (Vec3. 0.0 1.0 0.0) 0.5) 0.1))

  ;; normal calculator
  (def norm (r/make-normal scene 0.01))

  ;; ray marching fn
  (def ray-marching (r/make-ray-marching scene bg 0.02 max-depth 100 0.95 0.00001))

  ;; ambient occlusion
  (def ao (r/make-ao 0.3 0.65 5))

  (def shadow-f (r/make-soft-shadow 8 40 max-depth))

  (def light1 (v/normalize (Vec3. -2.0 1.0 0.5)))
  (def light2 (v/normalize (Vec3. 0.0 4.5 2.0)))

  (def light1-fn (r/make-light light1 scene (Vec3. 1.0 1.0 1.0) (Vec3. 1.0 1.0 1.0) 0.1))
  (def light2-fn (r/make-light light2 scene (Vec3. 1.0 1.0 1.0) (Vec3. 1.0 1.0 1.0) 0.1))
  
  ;; fog
  (def fog (r/make-distance-fog (.color bg) -0.01))

  (defn calc-color
    ""
    [ro rd ^double depth]
    (let [rm (ray-marching ro rd)
          ^HitData hit (rm 0)
          ^Material mat (.mat hit)
          hocc (rm 1)
          col (if (> (.d hit) max-depth)
                (.color mat)
                (let [^Vec3 pos (r/ray ro rd (.d hit))
                      ^Vec3 n (norm pos)
                      ^Material obj mat
                      occ (ao scene pos n)
                      col1 (light1-fn obj shadow-f n rd pos)
                      col2 (light2-fn obj shadow-f n rd pos)
                      col (v/add col1 col2)
                      col (v/mult col (m/lerp occ hocc 0.2))
                      col (fog (.d hit) col)]
                  (if (and (pos? (.reflection mat))
                           (pos? depth))
                    (v/interpolate col (calc-color pos (r/reflect rd n) (dec depth)) (.reflection mat))
                    col)))]
      col))


  (defn do-it
    ""
    [canvas]
    (dotimes [x w]
      (let [xx (m/norm x 0.0 w -2.0 2.0)]
        (dotimes [y h]
          (let [yy (m/norm y 0.0 h 2.0 -2.0)
                ^Vec3 rd (camera (v/normalize (Vec3. xx yy 1.0)))
                col (calc-color ro rd 1)]
            (set-color canvas (c/to-color (v/mult col 255)))
            (rect canvas x y 1 1)))))))

(with-canvas canvas
  (set-background 0 0 0)
  (do-it))
