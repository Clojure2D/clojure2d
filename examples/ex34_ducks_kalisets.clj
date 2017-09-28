;; http://www.algorithmic-worlds.net/blog/blog.php?Post=20110227
;; https://softologyblog.wordpress.com/2011/04/05/ducks-fractals/

;; Interactions, press:
;;
;; * n - new configuration and render
;; * z - zoom in
;; * x - zoom out
;; * w - change weighting scheme (randomize)
;; * c - change coloring scheme (randomize)
;; * SPACE - save image
;;
;; Click mouse to make new center point

(ns examples.ex34-ducks-kalisets
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.complex :as c]
            [clojure2d.math.vector :as v]
            [clojure2d.pixels :as p]
            [clojure2d.math.random :as r]
            [clojure.pprint :refer :all])
  (:import [clojure2d.math.vector Vec2 Vec4]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^int w 1000)
(def ^:const ^int h 1000)

(def ^:const title "Ducks and Kalisets")

(deftype Bounds [^double x1 ^double y1
                 ^double x2 ^double y2
                 ^double sx ^double sy]
  Object
  (toString [b] (str "[(" x1 ", " y1 "), (" x2 ", " y2 ")]")))

(defn make-bounds
  ""
  [^double x1 ^double y1 ^double x2 ^double y2]
  (Bounds. x1 y1 x2 y2 (* 0.5 (- x2 x1)) (* 0.5 (- y2 y1))))

;; Coloring functions
(def coloring-fns {:f1 (fn [nz z] (m/exp (* -6.0 ^double (v/mag nz))))
                   :f2 (fn [nz z] (m/exp (* -6.0 ^double (v/magsq nz))))
                   :f3 (fn [nz z] (m/exp (/ -6.0 ^double (v/mag (v/sub nz z)))))
                   :f4 (fn [nz z] (m/exp (/ -6.0 ^double (v/magsq (v/sub nz z)))))})

;; Fractal definitions
(def iter-fns {:ducks (fn [^Vec2 z c] (c/log (c/add (Vec2. (.x z) (m/abs (.y z))) c)))
               :ducksv1 (fn [^Vec2 z c] (c/log (c/sin (c/add (Vec2. (.x z) (m/abs (.y z))) c))))
               :ducksv2 (fn [^Vec2 z c]
                          (let [nz (c/add (Vec2. (.x z) (m/abs (.y z))) c)]
                            (c/log (v/sub nz (c/sec nz)))))
               :ducksv3 (fn [^Vec2 z c] (c/log (v/add (c/sin (Vec2. (.x z) (m/abs (.y z)))) c)))
               :ducksv4 (fn [^Vec2 z c] (c/log (c/sq (c/add (Vec2. (.x z) (m/abs (.y z))) c))))
               :kali (fn [z c] (v/add (v/div (v/abs z) (v/magsq z)) c))
               :kalisq (fn [^Vec2 z c] (v/add (v/div (v/abs z) (* (.x z) (.y z))) c))
               :kalidiv (fn [^Vec2 z c]
                          (let [az (v/abs z)]
                            (if (v/is-near-zero? az)
                              c/ZERO
                              (v/add (v/div (v/abs (c/div z (v/abs z))) (v/magsq z)) c))))
               :kali2 (fn [z c] (v/add (v/abs (c/reciprocal z)) c))
               :kali3 (fn [z c] (v/add (v/abs (c/reciprocal (c/sq z))) c))
               :kali4 (fn [z c] (v/add (c/sin (v/abs (c/reciprocal z))) c))
               :kali2l (fn [z c] (c/log (v/add (v/abs (c/reciprocal z)) c)))
               :kali3l (fn [z c] (c/log (v/add (v/abs (c/reciprocal (c/sq z))) c)))
               :kali4l (fn [z c] (c/log (v/add (c/sinh (v/abs (c/reciprocal z))) c)))
               :kalinewa (fn [z c]
                           (let [nz (v/abs (v/add (c/mult z c) c/ONE))]
                             (v/add nz (c/reciprocal nz))))
               :kalinew (fn [z c]
                          (let [nz (v/add (c/mult z c) c/ONE)]
                            (v/add nz (c/reciprocal nz))))
               :kaliabs (fn [z c] (v/add (v/abs (c/div z c)) c))
               :my (fn [z c] (c/sec (v/abs (v/add z c))))
               :my2 (fn [z c] (c/log (v/abs (c/div z c))))
               :my3 (fn [z c] (c/log (v/abs (c/div c z))))
               :my4 (fn [z c] (c/log (c/div (v/abs (c/mult c z)) (v/abs c))))
               :my5 (fn [z c] (v/add (v/abs (c/sq z)) c))
               :talism (fn [z c]
                         (let [az (v/abs z)]
                           (v/add (c/div (c/sq az) z) c)))
               :talis1 (fn [z c]
                         (let [az (v/abs z)]
                           (v/add (c/div (v/sub (c/sq az) az) z) c))) 
               :burningship-log (fn [^Vec2 z c] (c/log (v/add (c/sq (v/abs z)) c)))})

;; Randomize 'c' parameter
(def rand-c {:ducks #(Vec2. (r/drand 0.2 0.95) (r/drand -0.25 -1.5))
             :ducksv1 #(Vec2. (r/drand 0.2 0.95) (r/drand -0.25 -1.5))
             :ducksv2 #(Vec2. (r/drand -0.1 1.2) (r/drand -0.1 -1.5))
             :ducksv3 #(Vec2. (r/drand -1.5 1.5) (r/drand -1.5 0.5))
             :ducksv4 #(Vec2. (r/drand -1.5 1.5) (r/drand -1.5 0.0))
             :kali #(Vec2. (r/drand -0.1 -1.9) (r/drand -0.1 -1.9))
             :kalisq #(Vec2. (r/drand -1.5 1.5) (r/drand -1.5 1.5))
             :kalidiv #(Vec2. (r/drand -1.5 1.2) (r/drand -1.5 1.3))
             :kali2 #(Vec2. (r/drand -0.1 -1.9) (r/drand -1.9 0.0))
             :kali3 #(Vec2. (r/drand -1.2 1.2) (r/drand -1.2 1.2))
             :kali4 #(Vec2. (r/drand -1.5 0.2) (r/drand -1.2 0.2))
             :kali2l #(Vec2. (r/drand -0.5 1.0) (r/drand -1.5 0.1))
             :kali3l #(Vec2. (r/drand -0.5 1.0) (r/drand -1.5 0.1))
             :kali4l #(Vec2. (r/drand -0.5 1.0) (r/drand -1.5 0.1))
             :kalinewa #(Vec2. (r/drand -1.5 1.5) (r/drand -1.5 1.5))
             :kalinew #(Vec2. (r/drand -1.5 1.5) (r/drand -1.5 1.5))
             :kaliabs #(Vec2. (r/drand -0.8 -0.1) (r/drand -0.8 -0.1))
             :my #(Vec2. (r/drand -1.5 1.5) (r/drand -1.5 0))
             :my2 #(Vec2. (r/drand -2.5 2.5) (r/drand -1.5 1.5))
             :my3 #(Vec2. (r/drand -2.5 1.5) (r/drand -1.5 1.5))
             :my4 #(Vec2. (r/drand -2.5 1.5) (r/drand -1.5 1.5))
             :my5 #(let [x (r/drand -1 0.5)]
                     (if (pos? x)
                       (Vec2. x (r/drand -1.5))
                       (Vec2. x (r/drand -1.5 0.75))))
             :talism #(Vec2. (r/drand -1.5 1.5) (r/drand -1.5 1.5))
             :talis1 #(Vec2. (r/drand -1.5 1.5) (r/drand -1.5 1.5))
             :burningship-log #(Vec2. (r/drand 0.0 1.0) (r/drand -1.0 0.0))})

(def weight-fns [:unit :exp :log :rev])

(defn draw-ducks
  "Iterate and draw"
  [canvas {:keys [c-const coloring-fn iter-fn ^double mlt weight-fn ^Bounds bounds]}]
  (let [f (coloring-fns coloring-fn)
        iter (iter-fns iter-fn)
        factor (max 0.0 (- (m/log2 (.sx bounds))))
        limit (+ 20.0 (* 3.0 factor))
        ww (+ 7.0 factor)
        weightf (case weight-fn
                  :exp #(m/exp (- (/ ^int % ww)))
                  :log #(m/log (inc (/ (- limit ^int %) ww)))
                  :rev #(/ 1.0 (inc ^int %))
                  (constantly 1.0))]
    (dotimes [x w]
      (dotimes [y h]
        (let [xx (m/norm x 0.0 w (.x1 bounds) (.x2 bounds))
              yy (m/norm y 0.0 h (.y1 bounds) (.y2 bounds))
              m (loop [i (int 0)
                       z (Vec2. xx yy)
                       sum (double 0)
                       wsum (double 0)]
                  (let [^double w (weightf i)
                        nz (iter z c-const)]
                    (if (< i limit)
                      (recur (inc i)
                             nz 
                             (+ sum (* w ^double (f nz z)))
                             (+ wsum w))
                      (/ sum wsum))))
              col (* 255 (m/abs (m/qsin (* mlt m/TWO_PI (m/sqrt m)))))]
          (set-color canvas col col col)
          (rect canvas y x 1 1)))))
  canvas)

(defn make-random-config
  "Create random configuration"
  []
  (let [cfname (rand-nth (keys coloring-fns))
        itername (rand-nth (keys iter-fns))] 
    {:c-const ((rand-c itername))
     :coloring-fn cfname
     :mlt (r/drand 1 3)
     :iter-fn itername
     :weight-fn (rand-nth weight-fns)
     :bounds (make-bounds -2 -2 2 2)}))

(def canvas (make-canvas w h))
(def window (show-window {:canvas canvas
                          :window-name title
                          :fps 20
                          :state (make-random-config)}))

(defn draw-fractal
  "Draw fractal"
  [config]
  (pprint config)
  (with-canvas canvas
    (draw-ducks config)
    (p/set-canvas-pixels! (p/filter-channels p/normalize-filter nil (p/get-canvas-pixels canvas))))
  (println "done!")
  config)

(defn new-fractal
  "Create new config and draw fractal"
  []
  (draw-fractal (make-random-config)))

(defmethod key-pressed [title \n] [_ _]
  (new-fractal))

(defmethod key-pressed [title \space] [_ state]
  (save canvas (next-filename "results/ex34/" ".jpg"))
  state)

(defn scale-bounds
  [^double fact ^Bounds bounds]
  (let [midx (+ (.x1 bounds) (.sx bounds))
        midy (+ (.y1 bounds) (.sy bounds))
        nsx (* fact (.sx bounds))
        nsy (* fact (.sy bounds))]
    (make-bounds (- midx nsx) (- midy nsy)
                 (+ midx nsx) (+ midy nsy))))

(defmethod key-pressed [title \z] [_ config] 
  (draw-fractal (assoc config :bounds (scale-bounds 0.5 (:bounds config)))))

(defmethod key-pressed [title \x] [_ config]
  (draw-fractal (assoc config :bounds (scale-bounds 2.0 (:bounds config)))))

(defmethod key-pressed [title \c] [_ config]
  (draw-fractal (assoc config
                       :coloring-fn (rand-nth (keys coloring-fns))
                       :mlt (r/drand 1 3))))

(defmethod key-pressed [title \w] [_ config] 
  (draw-fractal (assoc config :weight-fn (rand-nth weight-fns))))

(defmethod mouse-event [title :mouse-pressed] [e config]
  (println config)
  (let [^Bounds cbounds (:bounds config)
        ^double nx (m/norm (mouse-y e) 0 w (.x1 cbounds) (.x2 cbounds))
        ^double ny (m/norm (mouse-x e) 0 h (.y1 cbounds) (.y2 cbounds))
        nbounds (make-bounds (- nx (.sx cbounds)) (- ny (.sy cbounds))
                             (+ nx (.sx cbounds)) (+ ny (.sy cbounds)))]
    (draw-fractal (assoc config :bounds nbounds))))

(new-fractal)
