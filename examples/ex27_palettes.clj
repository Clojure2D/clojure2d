(ns example.ex27-palettes
  (:require [clojure2d.core :refer :all]
            [clojure2d.extra.glitch :as g]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int width 800)
(def ^:const ^int height 400)

(def ^:const ^int halfw (int (/ width 2)))
(def ^:const ^int halfh (int (/ height 2)))

(def ^:const ^int ww (int (* 0.8 width)))
(def ^:const ^int hh (int (* 0.8 height)))
(def ^:const ^int bw (/ (- width ww) 2))
(def ^:const ^int bh (/ (- height hh) 2))

(def canvas (make-canvas width height))
(def window (show-window canvas "Palettes" width height 15))

(defn draw-palette
  ""
  [canvas values ^long box-size]
  (doseq [[^long id col] values]
    (set-color canvas col)
    (rect canvas id bh box-size hh)))

(defn do-it
  ""
  []
  (let [fpalette (g/color-reducer-machine)
        palette (:palette fpalette)
        box-size (int (/ ww (count palette)))
        values (map-indexed (fn [^long id v] [(+ bw (* id box-size)) v])
                            palette)]
    (println (str "Type: " (:type fpalette))) 
    (println (str "Size: " (count palette)))
    (println (str "Conf: " (:conf fpalette)))
    
    (with-canvas canvas
      (set-color 20 20 20)
      (rect 0 0 width halfh)
      (set-color 235 235 235)
      (rect 0 halfh width halfh)
      (draw-palette values box-size))

    fpalette))


(defmethod key-pressed ["Palettes" \space] [_]
  (do-it))

(do-it)
