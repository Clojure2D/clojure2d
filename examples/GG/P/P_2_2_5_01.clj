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
  (let [{:keys [pressed mouse-rect]} (get-state window)
        [new-x new-y new-r] (if pressed
                              [(r/drand (- (mouse-x window) (/ mouse-rect 2.0))
                                        (+ (mouse-x window) (/ mouse-rect 2.0)))
                               (r/drand (- (mouse-y window) (/ mouse-rect 2.0))
                                        (+ (mouse-y window) (/ mouse-rect 2.0)))
                               1.0]
                              [(r/drand max-radius, (- (width canvas) max-radius))
                               (r/drand max-radius, (- (height canvas) max-radius))
                               min-radius])
        intersect? (some #(let [[x y r] %
                                d (m/dist new-x new-y x y)]
                            (< d (+ new-r r))) lda)
        new-lda (if intersect?
                  lda
                  (let [[closest new-radius] (reduce #(let [new-radius (second %1)
                                                            [x y r] %2
                                                            d (m/dist new-x new-y x y)]
                                                        (if (> new-radius (- d r))
                                                          [%2 (- d r)]
                                                          %1))
                                                     [(first lda) (width canvas)]
                                                     (rest lda))]
                    (conj lda [new-x new-y (min new-radius max-radius) closest])))]
    
    (set-background canvas :white)
    
    (doseq [[x y r closest] new-lda]
      (-> canvas
          (set-color :black)
          (set-stroke 1.5)
          (ellipse x y (+ r r) (+ r r) true))
      (when-let [[cx cy] closest]
        (-> canvas
            (set-color 226 185 0)
            (set-stroke 0.75)
            (line x y cx cy))))

    (when pressed
      (-> canvas
          (set-color 255 200 0)
          (set-stroke 2.0)
          (crect (mouse-x window)
                 (mouse-y window)
                 mouse-rect mouse-rect true)))
    
    new-lda))

(def canvas (make-canvas 800 800))
(def window (show-window {:window-name wname
                          :canvas canvas
                          :draw-fn draw
                          :draw-state [[200.0 100.0 50.0 nil]]
                          :state {:pressed false
                                  :mouse-rect 30.0}}))

(defmethod mouse-event [wname :mouse-pressed] [_ s] (assoc s :pressed true))
(defmethod mouse-event [wname :mouse-released] [_ s] (assoc s :pressed false))

(defmethod key-pressed [wname virtual-key] [e s]
  (let [mouse-rect (:mouse-rect s)]
    (assoc s :mouse-rect (case (key-code e)
                           :up (+ mouse-rect 4.0)
                           :down (max 4.0 (- mouse-rect 4.0))
                           mouse-rect))))


