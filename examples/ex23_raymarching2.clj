(ns examples.ex23-raymarching2
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [clojure2d.color :as c]
            [clojure2d.extra.raymarching :as r])
  (:import [clojure2d.math.vector Vec2 Vec3]
           [java.awt Color]))

(set! *warn-on-reflection* true)

(def ^:const w 1200)
(def ^:const h 1200)

(def canvas (create-canvas w h))

(def window (show-window canvas "raymarching2" w h 15))

(defmethod key-pressed ["raymarching2" \space] [_]
  (save-canvas canvas "results/ex23/scene.jpg"))

(do
  (def max-depth 20.0)

  ;; ray origin
  (def ro (Vec3. 0.0 4.5 4.0))

  ;; camera
  (def camera (r/make-camera ro (Vec3. 0.0 2.0 2.0) 0.0))

  ;; object
  (def sphere (r/make-primitive :sphere 1 {:r 3.0}))
  (def box (r/make-primitive :box 1 {:box (Vec3. 2.5 2.5 2.5)}))

  ;; normal calculator
  (def norm (r/make-normal 0.01))

  ;; ray marching fn
  (def ray-marching (r/make-ray-marching 0.001 max-depth 200))

  ;; ambient occlusion
  (def ao (r/make-ao 0.3 0.65 5))

  (def shadow-f (r/make-soft-shadow 8 40 max-depth))

  (def light1 (v/normalize (Vec3. -2.0 1.0 0.0)))
  (def light2 (v/normalize (Vec3. 0.0 4.5 4.0)))

  (def light1-fn (r/make-light light1 0.904 555 (Vec3. 1.0 1.0 1.0) (Vec3. 1.0 1.0 1.0) 1.0 1.0 1.0))
  (def light2-fn (r/make-light light2 0.904 555 (Vec3. 1.0 1.0 1.0) (Vec3. 1.0 1.0 1.0) 1.0 1.0 1.0))

  ;; custom plane
  (defn fn-plane
    ""
    [^Vec3 p]
    (let [v (- (m/noise (* 0.03 (.x p)) (* 0.03 (.z p))) 1.5)]
      (Vec2. (- (.y p) v) 0.0)))

  ;; scene
  (def scene (r/op-union fn-plane 
                         (r/op-rotate (r/op-subtract box sphere) (Vec3. 0.0 1.0 0.0) 0.5)))

  (def scene-colors [(v/div (Vec3. 3 101 100) 255.0)
                     (v/div (Vec3. 13 23 52) 255.0)])
  (def background-color (v/div (Vec3. 232 221 203) 255.0))
  
    ;; fog
  (def fog (r/make-distance-fog background-color -0.01))

  (defn do-it
    ""
    [canvas]
    (dotimes [x w]
      (let [xx (m/norm x 0.0 w -2.0 2.0)]
        (dotimes [y h]
          (let [yy (m/norm y 0.0 h 2.0 -2.0)
                ^Vec3 rd (camera (v/normalize (Vec3. xx yy 1.0)))
                ^Vec3 t (ray-marching scene ro rd)
                col (if (> (.x t) max-depth)
                      background-color
                      (let [^Vec3 pos (r/ray ro rd (.x t))
                            ^Vec3 n (norm scene pos)
                            obj (int (.y t))
                            occ (ao scene pos n)
                            col (scene-colors obj)
                            sha1 (shadow-f scene pos light1)
                            sha2 (shadow-f scene pos light2)
                            sha (* 0.5 (+ sha1 sha2))
                            col1 (light1-fn col sha1 n pos)
                            col2 (light2-fn col sha2 n pos)
                            col (v/mult (v/add col1 col2) 0.5)
                            col (v/mult col occ)
                            ]
                        (fog (.x t) col)))]
            (set-color canvas (c/to-color3 (v/mult col 255)))
            (rect canvas x y 1 1)))))))

(with-canvas canvas
  ;(set-background Color/black)
  (do-it))

(def c (camera (v/normalize (Vec3. 0.1 -0.1 1.0))))
(def t (ray-marching scene ro c))

(def p (r/ray ro c (.x t)))
(def n (norm scene p))

(v/dot n light2)

(shadow-f scene p light1)
