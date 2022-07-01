(ns clerk.styles
  (:require [clojure2d.color :as c]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as view]
            [hiccup.util :as hutil]
            [clojure.string :as str]
            [fastmath.core :as m]))

(def color-styles
  [:style ".checkerboard {background-image: linear-gradient(45deg, #ddd 25%, transparent 25%),linear-gradient(-45deg, #ddd 25%, transparent 25%),linear-gradient(45deg, transparent 75%, #ddd 75%),linear-gradient(-45deg, transparent 75%, #ddd 75%);
                              background-size: 20px 20px;
                              background-position: 0 0, 0 10px, 10px -10px, -10px 0px;
                              z-index: 0;
                              margin-bottom: 1px;}
           .gs {float: left; width: 100%; height: 30px;}
           .gp {height: 30px; width: 30px; margin-right: 2px; float: left;}
           .gsfull {top: 0; left: 0; width: 100%; height: 100%; position: relative;}
           .csfnt {font-family: monospace; font-size: 12px;}
           span.csblk {display: block; padding: 5.2px; border: 1px solid; border-radius: 3px;}
           span.gs {border: 1px solid; border-radius: 3px;}
           .viewer {margin-top: 0.5rem;}
           .uppercase {margin-top: 0.5rem;}
           .viewer + .viewer { padding-top: 0.2rem; padding-bottom: 0.2rem; margin-top: 0.5rem;}
           h1, h2, h3, h4, p, ul {margin-bottom: 0.2rem !important; }"])

(defn color-spans [c]
  (let [str-color (if (< (c/luma c) 128.0) "white" "black")
        n (c/format-hex c)]
    [:div.checkerboard.csfnt
     [:span.csblk {:style {:background-color n
                           :border-color (c/format-hex (c/darken c))
                           :color str-color}} (str "[" (hutil/escape-html n) "] " (if (int? c) (unchecked-int c) c))]]))

#_(hutil/escape-html n)

(defn emit-css-gradient
  [lst]
  {:background-image (str "linear-gradient(90deg," (str/join "," lst) ")")})

(def to-percent (comp #(str % "%") double #(/ % 1000) int #(* % 100000.0)))

#_(defn palette->css-gradient
    [p]
    (let [cp (count p)        
          steps (mapcat (fn [c [g1 g2]]
                          [(str c " " g1)
                           (str c " " g2)])
                        (map c/format-hex p)
                        (partition 2 1 (map (comp to-percent #(m/norm % 0 cp 0.0 1.0)) (range (inc cp)))))]
      (emit-css-gradient steps)))

(defn palette->css-gradient
  [p]
  (for [col p
        :let [cn (c/format-hex col)]]
    [:div.checkerboard.gp [:span.gs {:title cn
                                     :style {:background-color cn
                                             :border-color (c/format-hex (c/darken col))}}]]))

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

(defn palette-div [emit-fn palette]
  [:div.gs.gsfull
   (emit-fn palette)])

(def palette->div (partial palette-div palette->css-gradient))
(def gradient->div (partial gradient-div gradient->css-gradient))

(defn get-divs
  [c]
  (cond
    (c/possible-color? c) (color-spans c)
    (c/possible-palette? c) (palette->div c)
    (fn? c) (gradient->div c)
    :else c))

;; palette icon
(defn 🎨 [& c]
  (if (= 1 (count c))
    (clerk/html (get-divs (first c)))
    (clerk/html (into [:div.flex.flex-col] (map get-divs c)))))

