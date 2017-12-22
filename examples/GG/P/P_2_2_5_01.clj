(ns GG.P.P-2-2-5-01
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.random :as r]
            [clojure2d.math :as m]))

(def wname "P_2_2_5_01")

(def ^:const ^double min-radius 3.0)
(def ^:const ^double max-radius 50.0)

(defn draw
  ""
  [canvas window _ lda]
  (let []))

(def canvas (make-canvas 800 800))
(def window (show-window {:window-name wname
                          :canvas canvas
                          :draw-fn draw
                          :draw-state [[200 100 50 [200 100 50]]]
                          :state false}))

(defmethod mouse-event [wname :mouse-pressed] [_ _] true)
(defmethod mouse-event [wname :mouse-released] [_ _] false)
