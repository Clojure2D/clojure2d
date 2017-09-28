(ns examples.NOC.ch08.simple-lsystem-8-6
  (:require [clojure2d.core :refer :all]))

(def canvas (make-canvas 200 200))
(def window (show-window {:canvas canvas
                          :window-name "Simple L-system 8_6"
                          :state {:string "A"
                                  :generation 0}}))

(with-canvas canvas
  (set-background :white)
  (set-color :black)
  (text "Click mouse to generate" 10 (- (height canvas) 20)))

(defmethod mouse-event [(:window-name window) :mouse-pressed] [_ {:keys [string generation] :as state}]
  (println (str "Generatation " generation ": " string))
  {:string (reduce #(str %1 (if (= %2 \A) "AB" "A")) "" string)
   :generation (inc generation)})
