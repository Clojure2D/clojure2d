(ns clerk.styles
  (:require [clojure2d.color :as c]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as view]
            [clojure.string :as str]
            [fastmath.core :as m]))

(def color-styles
  [:style ".checkerboard {background-image: linear-gradient(45deg, #ddd 25%, transparent 25%),linear-gradient(-45deg, #ddd 25%, transparent 25%),linear-gradient(45deg, transparent 75%, #ddd 75%),linear-gradient(-45deg, transparent 75%, #ddd 75%);
                              background-size: 20px 20px;
                              background-position: 0 0, 0 10px, 10px -10px, -10px 0px;:z-index -1;}
           .gs {float: left; width: 100%; height: 30px;}
           .gsfull {top: 0; left: 0; width: 100%; heigth: 100%; position: relative;}
           .csfnt {font-family: monospace; font-size: 12px; padding: 0px;}
           span.csblk {display: block; padding: 5.2px;}
           .viewer-code {margin-top: 10px;}"])

(defn color-spans [c]
  (let [c (view/value c)
        str-color (if (< (c/luma c) 128.0) "white" "black")
        n (c/format-hex c)]
    [:div.checkerboard.csfnt
     [:span.csblk {:style {:background-color n
                           :color str-color}} (str "[" n "] " (if (int? c) (unchecked-int c) c))]]))

(defn emit-css-gradient
  [lst]
  {:background-image (str "linear-gradient(90deg," (str/join "," lst) ")")})

(def to-percent (comp #(str % "%") double #(/ % 1000) int #(* % 100000.0)))

(defn palette->css-gradient
  [p]
  (let [cp (count p)        
        steps (mapcat (fn [c [g1 g2]]
                        [(str c " " g1)
                         (str c " " g2)])
                      (map c/format-hex p)
                      (partition 2 1 (map (comp to-percent #(m/norm % 0 cp 0.0 1.0)) (range (inc cp)))))]
    (emit-css-gradient steps)))

(def pcts-doubles (map #(m/norm % 0 29 0.0 1.0) (range 30)))
(def pcts-strs (map to-percent pcts-doubles))
(def pcts (map vector pcts-doubles pcts-strs))

(defn gradient->css-gradient
  [f]
  (emit-css-gradient (for [[p ps] pcts]
                       (str (c/format-hex (f p)) " " ps))))

(defn gradient-div [emit-fn gradient-or-palette]
  [:div.checkerboard.gs.gsfull
   [:span.gs {:style (emit-fn gradient-or-palette)}]])

(def palette->div (partial gradient-div palette->css-gradient))
(def gradient->div (partial gradient-div gradient->css-gradient))

;; palette icon
(defn 🎨
  [c]
  (clerk/html (cond
                (c/possible-color? c) (color-spans c)
                (c/possible-palette? c) (palette->div c)
                (fn? c) (gradient->div c)
                :else c)))

