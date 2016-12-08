(ns examples.ex24-sdf
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [clojure2d.color :as c]
            [clojure2d.extra.raymarching :as r])
  (:import [clojure2d.math.vector Vec2 Vec3]
           [java.awt Color]))

(set! *warn-on-reflection* true)

(def ^:const w 600)
(def ^:const h 600)

(def canvas (create-canvas w h))

(def window (show-window canvas "sdf" w h 15))

(defmethod key-pressed ["sdf" \space] [_]
  (save-canvas canvas "results/ex24/scene.jpg"))

(do
  (def max-depth 20.0)

  ;; ray origin
  (def ro (Vec3. 0.0 5.0 3.0))

  ;; camera
  (def camera (r/make-camera ro (Vec3. 0.0 4.0 1.0) 0.0))

  ;; object
  (def sphere (r/make-primitive :sphere 1 {:r 3.0}))
  (def box    (r/make-primitive :box 2 {:box (Vec3. 3.0 3.0 3.0)}))
  (def torus  (r/make-primitive :torus 3 {:small-radius 0.3
                                          :large-radius 3.0}))
  
  (def box-sphere (r/op-subtract box (r/make-primitive :sphere 1 {:r 3.5})))

  (def lrp    (r/op-transform (r/op-rotate box-sphere (Vec3. 0.3 1.1 0.3) -0.4) (Vec3. 0.0 1.0 -3.0)))

  (def plane (r/make-primitive :plane 0 {:normal (Vec3. 0.0 1.0 0.0)
                                         :dist-from-origin 1.0}))

  ;; normal calculator
  (def norm (r/make-normal 0.01))

  ;; ray marching fn
  (def ray-marching (r/make-ray-marching 0.01 max-depth 100))

  ;; ambient occlusion
  (def ao (r/make-ao 0.5 0.65 5))

  (def shadow-f (r/make-soft-shadow 8 20 max-depth))

  (def light1 (v/normalize (Vec3. -2.0 2.0 1.0)))
  (def light2 (v/normalize (Vec3. 0.0 1.0 1.0)))

  (let [^Vec3 a1 (Vec3. 1.0 1.0 1.0)
        ^Vec3 a2 (Vec3. -1.0 -1.0 1.0)
        ^Vec3 a3 (Vec3. 1.0 -1.0 -1.0)
        ^Vec3 a4 (Vec3. -1.0 1.0 -1.0)
        scale 3.0
        iters 5
        dscale (dec scale)
        fact (m/pow scale (- iters))]
   (defn sierp [p]
     (loop [n (int 0)
            z p]
       (if (< n iters)
           (let [d1 (v/mag (v/sub z a1))
                 d2 (v/mag (v/sub z a2))
                 d3 (v/mag (v/sub z a3))
                 d4 (v/mag (v/sub z a4))
                 c [a1 d1]
                 c (if (< d2 (c 1)) [a2 d2] c)
                 c (if (< d3 (c 1)) [a3 d3] c)
                 c (if (< d4 (c 1)) [a4 d4] c)]
             (recur (unchecked-inc n)
                    (v/sub (v/mult z scale)
                           (v/mult (c 0) dscale))))
           (Vec2. (* fact (v/mag z)) 2)
           ))))

  (let [bailout 2.0
        iters 5
        power 8
        dpower (dec power)]
      (defn mandelbulb
        ""
        [p]
        (loop [^Vec3 z p
               dr 1.0
               r (v/mag z)
               i (int 0)]
          (if (and 
               (< i iters)
               (< r bailout))
            (let [theta (* power (m/acos (/ (.z z) r)))
                  phi (* power (m/atan (/ (.y z) (.x z))))
                  zr (m/pow r power)
                  ^Vec3 nz (v/from-polar (Vec3. zr theta phi))
                  newz (v/add p nz)]
              (recur newz
                     (inc (* (m/pow r dpower) power dr))
                     (v/mag newz)
                     (unchecked-inc i)))
            (Vec2. (/ (* (m/log r) r)
                      (* 2.0 dr))
                   1)))
        ))


  ;; scene
  (def scene (r/op-transform (r/op-scale (r/op-rotate lrp (Vec3. 0.0 1.0 0.0) m/HALF_PI) 3.0) (Vec3. 0.0 5.0 -1.0 )))

(comment conj (map #(r/op-rotate % (Vec3. 0.0 1.0 0.0) 0.0)
                          [sphere box lrp]) plane)

  (def scene-colors [(v/div (Vec3. 222 223 225) 255.0)
                     (v/div (Vec3. 223 223 252) 255.0)
                     (v/div (Vec3. 113 223 52) 255.0)
                     (Vec3. 1.0 0.0 0.0)])
  (def background-color (v/div (Vec3. 232 221 203) 255.0))
  
    ;; fog
  (def fog (r/make-distance-fog background-color -0.001)))

(dotimes [x w]
  (let [xx (m/norm x 0.0 w -3.0 3.0)]
    (dotimes [y h]
      (let [yy (m/norm y 0.0 h 3.0 -3.0)
            ^Vec3 rd (camera (v/normalize (Vec3. xx yy 1.0)))
            ^Vec3 t (ray-marching scene ro rd)
            col (if (> (.x t) max-depth)
                  background-color
                  (let [^Vec3 pos (r/ray ro rd (.x t))
                        ^Vec3 n (norm scene pos)
                        obj (int (.y t))
                        occ (ao scene pos n)
                        sha1 (shadow-f scene pos light1)
                        dif1 (* (m/constrain (v/dot n light1) 0.0 1.0) sha1)
                        sha2 (shadow-f scene pos light2)
                        dif2 (* (m/constrain (v/dot n light2) 0.0 1.0) sha2)                       
                        col (scene-colors obj)
                        col (v/mult col (* 0.6 dif1 dif2))
                        col (v/add col (Vec3. occ occ occ))
                        ]
                    (fog (.x t) col)))]
        (with-canvas canvas
          (set-color (c/to-color3 (v/mult col 255)))
          (rect x y 1 1))))))
