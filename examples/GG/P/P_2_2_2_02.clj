(ns examples.GG.P.P-2-2-2-02
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.color :as c]))

(def ^:const ^double angle-count 7.0)
(def ^:const ^double step-size 3.0)
(def ^:const ^double min-length 10.0)
(def ^:const ^double dweight 50.0)
(def ^:const ^double dstroke 4.0)

(def wname "P_2_2_2_02")

(def hsb-mode (c/make-color-converter c/from-HSB 360 100 100 100))

(defn get-random-angle
  ""
  [the-direction]
  (let [a (* (/ 90.0 angle-count) (m/floor (+ 0.5 (r/drand (- angle-count) angle-count))))]
    (case the-direction
      :north (- a 90.0)
      :east a
      :south (+ a 90.0)
      :west (+ a 180.0)
      0.0)))

(defn draw-helper
  "Recurrent version of draw (to loop)"
  [canvas mode ^long times [^double posx ^double posy ^double posx-cross ^double posy-cross ^double angle direction :as all]]
  (if (neg? times)
    all
    (let [posx (+ posx (* step-size (m/cos (m/radians angle))))
          posy (+ posy (* step-size (m/sin (m/radians angle))))
          [direction reached-border] (cond
                                       (<= posy 5.0) [:south true]
                                       (>= posx (- (width canvas) 5.0)) [:west true]
                                       (<= posx 5.0) [:east true]
                                       (>= posy (- (height canvas) 5.0)) [:north true]
                                       :else [direction false])
          px (int posx)
          py (int posy)
          [angle posx-cross posy-cross] (if (or (< (c/to-luma (get-pixel canvas px py)) 255)
                                                reached-border) 
                                          (let [angle (get-random-angle direction) 
                                                distance (m/dist posx posy posx-cross posy-cross)]
                                            (when (>= distance min-length)
                                              (-> canvas
                                                  (set-stroke (/ distance dweight))
                                                  (set-color (case mode
                                                               1 :black
                                                               2 (hsb-mode (c/make-color 52 100 (/ distance dstroke) 100))
                                                               3 (hsb-mode (c/make-color 192 100 64 (/ distance dstroke)))))
                                                  (line posx posy posx-cross posy-cross)))
                                            [angle posx posy])
                                          [angle posx-cross posy-cross])]
      (recur canvas mode (dec times) [posx posy posx-cross posy-cross angle direction]))))


(defn draw
  ""
  [canvas window _ state]
  (draw-helper canvas (get-state window) (max 1 (mouse-x window)) state))

(def canvas (make-canvas 600 600))

(with-canvas-> canvas (set-background :white))

(def window (let [posx (r/irand (width canvas))
                  posy 5.0]
              (show-window {:window-name wname
                            :canvas canvas
                            :draw-fn draw
                            :state 1
                            :draw-state [posx posy posx posy 
                                         (get-random-angle :south)
                                         :south]})))

(defmethod key-released [wname \backspace] [_ s]
  (with-canvas-> canvas (set-background :white))
  s)

(defmethod key-released [wname \1] [_ _] 1)
(defmethod key-released [wname \2] [_ _] 2)
(defmethod key-released [wname \3] [_ _] 3)
