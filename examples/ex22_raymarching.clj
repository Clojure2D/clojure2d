;; https://www.shadertoy.com/view/XsX3RB
;; http://www.iquilezles.org/www/index.htm

;; low speed cpu version

(ns examples.ex22-raymarching
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :refer :all]
            [clojure2d.math.vector :as v]
            [clojure2d.color :as c]
            [clojure2d.extra.variations :as var])
  (:import [clojure2d.math.vector Vec2 Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^long w 1000)
(def ^:const ^long h 1000)

(def canvas (create-canvas w h))

(def window (show-window canvas "raymarching" 15 nil))

(defmethod key-pressed ["raymarching" \space] [_ _]
  (save canvas "results/ex22/scene.jpg"))

(def ^:const ^double mint 0.01) ;; minimum ray distance 
(def ^:const ^double maxt 30.0) ;; maximum ray distance

(def ^Vec3 sun-light (v/normalize (Vec3. -300.7 0.4 -200.0)))

(def ^Vec3 fog-color (v/div (Vec3. 200 200 200) 255.0))
(def ^Vec3 sky-color (v/div (Vec3. 135 206 250) 255.0))
(def ^Vec3 sun-color (v/div (Vec3. 253 184 19) 255.0))
(def ^Vec3 glow-color (v/div (Vec3. 255 153 51) 255.0))

(def ^Vec3 sun-lin (Vec3. 1.2 0.5 1.8)) ;; material color

(do
  (def ^Vec3 ro (Vec3. 0.0 5.0 10.0)) ;; ray origin
  (def ^Vec3 ch (Vec3. 0.0 2.5 4.0)) ;; camera heading
  (def camera-tilt 0.0) ;; tilt camera

  (defn set-camera
    ""
    [ta cr]
    (let [cw (v/normalize (v/sub ta ro))
          cp (Vec3. (m/sin cr) (m/cos cr) 0.0)
          cu (v/normalize (v/cross cw cp))
          cv (v/normalize (v/cross cu cw))]
      [cu cv cw]))

  (let [[c1 c2 c3] (set-camera ch camera-tilt)]
    (def ^Vec3 cam1 c1)
    (def ^Vec3 cam2 c2)
    (def ^Vec3 cam3 c3)))

(defn cast-ray
  ""
  ^double [rd f]
  (loop [i (int 0)
         t mint]
    (let [^Vec3 p (v/add ro (v/mult rd t))
          diffh (- (.y p) ^double (f (.x p) (.z p)))]
      (if (or (> t maxt)
              (> i 50)
              (< diffh (* 0.001 t)))
        t
        (recur (unchecked-inc i) (+ diffh t))))))

(def ^:const ^double k 16.0)

(defn softshadow
  ""
  [pos rd f]
  (loop [i (int 0)
         res 1.0
         t mint]
    (let [^Vec3 p (v/add pos (v/mult rd t))
          h (max 0.0 ^double (f (.x p) (.z p)))
          newres (min res (/ (* k h) t))]
      (if (or (< h 0.0001)
              (> i 20))
        (m/constrain newres 0.0 1.0)
        (recur (unchecked-inc i) newres (+ t (m/constrain h 0.02 0.5)))))))


(def terrain-f (comp (var/make-variation :sinusoidal 1.2 {})
                     (var/make-variation :auger 0.7 {})))

(defn terrain
  ""
  [^double x ^double y]
  (let [^Vec2 v (terrain-f (Vec2. (+ x 0.5) y))]
    (* 3.5 ^double (noise (* 0.08 (.x v)) (* 0.08 (.y v))))))

(defn normal
  ""
  [f ^Vec3 v ^double t]
  (let [eps (max 0.002 (* 0.0001 t))]
    (v/normalize (Vec3. (- ^double (f (- (.x v) eps) (.z v))
                           ^double (f (+ (.x v) eps) (.z v)))
                        (+ eps eps)
                        (- ^double (f (.x v) (- (.z v) eps))
                           ^double (f (.x v) (+ (.z v) eps)))))))

(defn calc-fog
  ""
  [^double t col bcol]
  (let [fo (- 1.0 (m/exp (* -0.01 t t)))
        fbcol (v/interpolate fog-color bcol fo)]
    (v/interpolate col fbcol fo)))

(let [v3 (Vec3. 3.0 3.0 3.0)]
    (defn contrast
      ""
      [col]
      (let [s (v/mult (v/add v3 (v/mult col 2.0)) 0.1)] 
        (v/add (v/mult col 0.9)
               (v/emult col (v/emult s col))))))

(defn gamma
  ""
  [col]
  (v/applyf col #(m/pow (m/constrain ^double % 0.0 1.0) 1.5)))

(defn desaturate
  ""
  [col]
  (let [luma (c/to-luma col)]
    (v/interpolate col (Vec3. luma luma luma) 0.2)))

(defn reflect
  ""
  [I N]
  (v/sub I (v/mult N (* 2.0 ^double (v/dot N I)))))

(defn smoothstep
  ""
  ^double [edge0 edge1 ^double x]
  (let [t (m/norm x edge0 edge1)]
    (* t t (- 3.0 (* 2.0 t)))))

(dotimes [x w]
  (let [xx (m/norm x 0.0 w -2.0 2.0)]
    (dotimes [y h]
      (let [yy (m/norm y h 0.0 -2.0 2.0)
            point (v/normalize (Vec3. xx yy 1.0)) ;; screen point
            ^Vec3 rd (Vec3. (v/dot cam1 point)
                            (v/dot cam2 point)
                            (v/dot cam3 point)) ;; view vector
            t (cast-ray rd terrain) ;; distance to terrain
            s (m/constrain ^double (v/dot rd sun-light) 0.0 1.0) ;; sun direction
            bcol (-> sun-color
                     (v/mult (* 0.2 (m/pow s 6.0)))
                     (v/add (v/sub sky-color (v/mult (Vec3. (.y rd) (.y rd) (.y rd)) 0.9)))
                     (v/mult 0.9)) ;; sun color
            col (if (< t maxt) ;; terrain
                  (let [^Vec3 pos (v/add ro (v/mult rd t))
                        ^Vec3 nor (normal terrain pos t)
                        hh (- 1.0 (smoothstep -2.0 1.0 (.y pos)))
                        sun (m/constrain ^double (v/dot nor sun-light) 0.0 1.0)
                        sha (if (> sun 0.01) (softshadow pos sun-light terrain) 0.0)
                        sky (+ 0.5 (* 0.5 (.y nor)))

                        lin (v/emult (v/mult sun-lin sun) (Vec3. sha (m/pow sha 1.2) (m/pow sha 1.5)))]
                    (calc-fog t lin bcol))
                  bcol)
            col (v/add col (v/mult glow-color (* (* 0.2 s s) (m/constrain (/ (+ (.y rd) 0.4) 0.4) 0.0 1.0)))) ;; sun glow
            c (-> col
                  gamma
                  contrast
                  desaturate
                  (v/mult 255)
                  c/to-color)]
        (with-canvas canvas
          (set-color c)
          (rect x y 1 1))))))
