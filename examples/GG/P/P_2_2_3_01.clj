(ns GG.P.P-2-2-3-01
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v]))

(def wname "P_2_2_3_01")

(def ^:const ^int form-resolution 15)
(def ^:const ^double step-size 2.0)
(def ^:const ^double init-radius 150.0)
(def ^:const ^double angle (m/radians (/ 360.0 form-resolution)))


(defn draw
  "Setup"
  [canvas window _ [center-x center-y points :as all]] 
  (let [{:keys [filled? freezed? pressed?]} (get-state window)]
    (if freezed? all
        (let [mx (mouse-x window)
              my (mouse-y window)
              [center-x center-y points :as all] (if-not pressed?
                                                   [(+ center-x (* 0.01 (- mx center-x)))
                                                    (+ center-y (* 0.01 (- my center-y)))
                                                    (map #(v/add % (v/vec2 (r/drand (- step-size) step-size)
                                                                           (r/drand (- step-size) step-size))) points)]
                                                   (let [radius (* init-radius (r/drand 0.5 1.0))]
                                                     [mx my (for [i (range form-resolution)]
                                                              (v/vec2 (* radius (m/cos (* i angle)))
                                                                      (* radius (m/sin (* i angle)))))]))
              vcenter (v/vec2 center-x center-y)
              centered-points (map #(v/add % vcenter) points)]
          (set-stroke canvas 0.7)
          (set-color canvas :black 50)
          (path-bezier canvas centered-points true true)
          (when filled?
            (let [r (r/drand 255)]
              (set-color canvas r r r)
              (path-bezier canvas centered-points true false)))
          all))))

(def canvas (make-canvas 1000 800))

(with-canvas-> canvas (set-background :white))

(def window (show-window {:canvas canvas
                          :window-name wname
                          :draw-fn draw 
                          :draw-state [(/ (width canvas) 2)
                                       (/ (height canvas) 2)
                                       (for [i (range form-resolution)]
                                         (v/vec2 (* init-radius (m/cos (* i angle)))
                                                 (* init-radius (m/sin (* i angle)))))]
                          :state {:filled? false
                                  :freezed? false
                                  :pressed? false}}))

(defmethod mouse-event [wname :mouse-pressed] [_ s] (assoc s :pressed? true))
(defmethod mouse-event [wname :mouse-released] [_ s] (assoc s :pressed? false))

(defmethod key-released [wname \f] [_ s] (assoc s :freezed? (not (:freezed? s))))
(defmethod key-released [wname \1] [_ s] (assoc s :filled? false))
(defmethod key-released [wname \2] [_ s] (assoc s :filled? true))

(defmethod key-released [wname \backspace] [_ s] (with-canvas-> canvas (set-background :white)) s)
