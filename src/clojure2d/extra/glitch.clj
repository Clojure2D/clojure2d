(ns clojure2d.extra.glitch
  (:require [clojure2d.math :as m]
            [clojure2d.pixels :as p]
            [clojure2d.core :refer :all]
            [clojure2d.math.vector :as v]
            [clojure2d.extra.signal :as s]
            [clojure2d.color :as c])
  (:import [clojure2d.pixels Pixels]
           [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; Simple 2d SLITSCAN

(def freqs (vec (map #(bit-shift-left 1 %) (range 16))))
(def amps (vec (map #(/ 1.0 %) freqs)))

(defn slitscan-random-setup
  ""
  ([n]
   (let [f (fn []
             (let [r (m/irand 4)]
               {:wave (rand-nth s/waves)
                :freq (freqs r)
                :amp (amps r)
                :phase (m/drand)}))]
     (filter (fn [_] (m/brand 0.8)) (repeatedly n f))))
  ([]
   (slitscan-random-setup (m/irand 2 6))))

(defn make-slitscan-waves
  ""
  ([waves]
   (s/make-sum-wave (map #(s/make-wave (:wave %) (:freq %) (:amp %) (:phase %)) waves)))
  ([]
   (make-slitscan-waves (slitscan-random-setup))))

(defn slitscan
  ""
  [fx fy ch ^Pixels p x y]
  (let [sx (/ 1.0 (.w p))
        sy (/ 1.0 (.h p))
        shiftx (* 0.5 (.w p) (fx (* x sx)))
        shifty (* 0.5 (.h p) (fy (* y sy)))
        xx (mod (int (+ x (.w p) shiftx)) (.w p))
        yy (mod (int (+ y (.h p) shifty)) (.h p))]
      (p/get-value p ch xx yy)))

(defn make-slitscan-filter
  ""
  ([]
   (partial p/filter-channel-xy (partial slitscan (make-slitscan-waves) (make-slitscan-waves))))
  ([fx fy]
   (partial p/filter-channel-xy (partial slitscan fx fy))))

;; channel shifts

(defn make-shift-channels-filter
  ""
  [amount h v]
  (let [mv (fn [_] amount)
        zr (fn [_] 0.0)]
   (make-slitscan-filter (if h mv zr) (if v mv zr))))

;; mirrorimage

(defn- mi-draw-point
  ""
  ([ch target source oldx oldy newx newy sx sy]
   (p/set-value target ch (+ newx sx) (+ newy sy) (p/get-value source ch (+ oldx sx) (+ oldy sy))))
  ([ch target source oldx oldy newx newy]
   (p/set-value target ch newx newy (p/get-value source ch oldx oldy))))

(defn- mi-do-horizontal
  ""
  [t ch target ^Pixels source]
  (dotimes [y (/ (.h source) 2.0)]
    (dotimes [x (.w source)]
      (if t
        (mi-draw-point ch target source x y x (- (.h source) y 1))
        (mi-draw-point ch target source x (- (.h source) y 1) x y)))))

(defn- mi-do-vertical
  ""
  [t ch target ^Pixels source]
  (dotimes [x (/ (.w source) 2.0)]
    (dotimes [y (.h source)]
      (if t
        (mi-draw-point ch target source x y (- (.w source) x 1) y)
        (mi-draw-point ch target source (- (.w source) x 1) y x y)))))

(defn- mi-do-diag-ul
  ""
  [t shift? ch target ^Pixels source]
  (let [size (min (.w source) (.h source))
        tx (if shift? (- (.w source) size) 0)
        ty (if shift? (- (.h source) size) 0)]
    (dotimes [y size]
      (dotimes [x (inc y)]
        (condp = (int t)
          0 (mi-draw-point ch target source x y y x tx ty)
          1 (mi-draw-point ch target source y x x y tx ty)
          2 (mi-draw-point ch target source x y (- size x 1) (- size y 1) tx ty)
          3 (mi-draw-point ch target source y x (- size y 1) (- size x 1) tx ty))))))

(defn- mi-do-diag-ur
  ""
  [t shift? ch target ^Pixels source]
  (let [size (min (.w source) (.h source))
        tx (if shift? (- (.w source) size) 0)
        ty (if shift? (- (.h source) size) 0)]
    (dotimes [y size]
      (loop [x (int (dec size))]
        (when (>= x (- size y 1))
          (condp = (int t)
            0 (mi-draw-point ch target source x y (- size y 1) (- size x 1) tx ty)
            1 (mi-draw-point ch target source (- size y 1) (- size x 1) x y tx ty)
            2 (mi-draw-point ch target source x y (- size x 1) (- size y 1) tx ty)
            3 (mi-draw-point ch target source (- size x 1) (- size y 1) x y tx ty))
          (recur (unchecked-dec x)))))))

(defn- mi-do-diag-rect
  ""
  [t l ch target ^Pixels source]
  (dotimes [y (.h source)]
    (let [d (if t
              (m/norm y 0 (.h source) 0 (.w source))
              (m/norm y 0 (.h source) (.w source) 0))]
      (dotimes [x d]
        (if l
          (mi-draw-point ch target source (- (.w source) x 1) (- (.h source) y 1) x y)
          (mi-draw-point ch target source x y (- (.w source) x 1) (- (.h source) y 1)))))))

(def mirror-types {:U    (partial mi-do-horizontal true)
                :D    (partial mi-do-horizontal false)
                :L    (partial mi-do-vertical true)
                :R    (partial mi-do-vertical false)
                :DL   (partial mi-do-diag-ul 0 false)
                :UR   (partial mi-do-diag-ul 1 false)
                :DL2  (partial mi-do-diag-ul 2 false)
                :UR2  (partial mi-do-diag-ul 3 false)
                :SDL  (partial mi-do-diag-ul 0 true)
                :SUR  (partial mi-do-diag-ul 1 true)
                :SDL2 (partial mi-do-diag-ul 2 true)
                :SUR2 (partial mi-do-diag-ul 3 true)
                :DR   (partial mi-do-diag-ur 0 false)
                :UL   (partial mi-do-diag-ur 1 false)
                :DR2  (partial mi-do-diag-ur 2 false)
                :UL2  (partial mi-do-diag-ur 3 false)
                :SDR  (partial mi-do-diag-ur 0 true)
                :SUL  (partial mi-do-diag-ur 1 true)
                :SDR2 (partial mi-do-diag-ur 2 true)
                :SUL2 (partial mi-do-diag-ur 3 true)
                :RUR  (partial mi-do-diag-rect true true)
                :RDR  (partial mi-do-diag-rect false true)
                :RDL  (partial mi-do-diag-rect true false)
                :RUL  (partial mi-do-diag-rect false false)})


(defn make-mirror-filter
  ""
  [t]
  (t mirror-types))

;;

(defn make-slitscan2-filter
  "f: Vec2 -> Vec2 (use variation)
   r: value 1.0-3.0"
  ([f r]
   (let [r- (- r)]
     (fn [ch t ^Pixels p]
       (dotimes [y (.h p)]
         (let [yv (m/norm y 0 (.h p) r- r)]
           (dotimes [x (.w p)]
             (let [xlerp (m/norm x 0 (.w p) 0.0 1.0)
                   v1 (f (Vec2. r- yv))
                   v2 (f (Vec2. r yv))
                   ^Vec2 vv (v/interpolate v1 v2 xlerp)
                   xx (int (m/norm (.x vv) r- r 0.0 (.w p)))
                   yy (int (m/norm (.y vv) r- r 0.0 (.h p)))]
               (p/set-value t ch x y (p/get-value p ch xx yy)))))))))
  ([f]
   (make-slitscan2-filter f 2.0)))

;; blend machine

(defn random-blend-get-cs
  "Return colorspace or nil"
  []
  (if (m/brand 0.85) (rand-nth c/colorspaces-names) nil))

(defn random-blend
  "Do random blend of two pixels, use random colorspace"
  [p1 p2]
  (let [[p1 p2] (if (m/brand 0.5) [p1 p2] [p2 p1]) ; switch images
        cs1 (random-blend-get-cs) ; let's convert to some colorspace (or leave rgb)
        cs2 (if (m/brand 0.2) (random-blend-get-cs) cs1) ; maybe different cs on second image?
        outcs (if (m/brand 0.2) (random-blend-get-cs) cs1) ; maybe some random colorspace on output?
        bl1 (if (m/brand 0.75) (rand-nth c/blends-names) nil) ; ch1 blend
        bl2 (if (m/brand 0.75) (rand-nth c/blends-names) nil) ; ch2 blend
        bl3 (if (m/brand 0.75) (rand-nth c/blends-names) nil) ; ch3 blend
        result (p/compose-channels bl1 bl2 bl3 nil
                                   (if cs1 (p/filter-colors ((cs1 c/colorspaces) 0) p1) p1)
                                   (if cs2 (p/filter-colors ((cs2 c/colorspaces) 0) p2) p2))]
    (println [cs1 cs2 outcs bl1 bl2 bl3])
    (if outcs
      (p/filter-colors ((outcs c/colorspaces) 1) result)
      result)))


