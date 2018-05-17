(ns generateme.letters
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [clojure2d.color :as c]
            [fastmath.stats :as stat])
  (:import [fastmath.vector Vec2]))

(def ^:const ^int w 800)
(def ^:const ^int hw (/ w 2))

(def ^Vec2 midpoint (v/vec2 hw hw))

(def ^:const ^double step (/ 1.0 120.0))

(def cb (c/to-color :red))

(defn draw-letter
  "Draw letter in the middle of canvas"
  [canvas letter]
  (-> canvas
      (set-background :white)
      (set-color cb)
      (set-font-attributes (* 0.7 w) :bold)
      (text (str letter) hw (- (+ (font-height canvas) hw) (font-ascent canvas)) :center)))

(defn cast-ray
  "Cast ray from given angle towards midpoint"
  [canvas angle]
  (let [start (v/add midpoint (v/from-polar (v/vec2 hw angle)))
        hit (loop [x 0.0]
              (let [pos (v/interpolate start midpoint x)
                    c (get-pixel canvas (long (pos 0)) (long (pos 1)))]
                (if (or (> x 1.0) (= c cb)) pos (recur (+ x step)))))]
    (v/dist start hit)))

(defn cast-rays
  ""
  [canvas astep]
  (let [points (map #(cast-ray canvas %) (range 0 m/TWO_PI astep))
        [mn mx] (stat/extent points)
        cnt (count points)
        p (map #(v/vec2 (m/norm %1 0 cnt 100 700)
                        (- hw (* %2 100.0))) (range cnt) (map #(m/norm % mn mx) points))]
    (set-color canvas :black 200)
    (path-bezier canvas p)))

(def c (canvas w w))

(into [] (.getAvailableFontFamilyNames (java.awt.GraphicsEnvironment/getLocalGraphicsEnvironment)))
;; => ["Agency FB" "Algerian" "Arial" "Arial Black" "Arial Narrow" "Arial Rounded MT Bold" "Arial Unicode MS" "Bahnschrift" "Baskerville Old Face" "Bauhaus 93" "Bell MT" "Berlin Sans FB" "Berlin Sans FB Demi" "Bernard MT Condensed" "Blackadder ITC" "Bodoni MT" "Bodoni MT Black" "Bodoni MT Condensed" "Bodoni MT Poster Compressed" "Book Antiqua" "Bookman Old Style" "Bookshelf Symbol 7" "Bradley Hand ITC" "Britannic Bold" "Broadway" "Brush Script MT" "Calibri" "Calibri Light" "Californian FB" "Calisto MT" "Cambria" "Cambria Math" "Candara" "Castellar" "Centaur" "Century" "Century Gothic" "Century Schoolbook" "Chiller" "Colonna MT" "Comic Sans MS" "Consolas" "Constantia" "Cooper Black" "Copperplate Gothic Bold" "Copperplate Gothic Light" "Corbel" "Courier New" "Curlz MT" "Delphine" "Dialog" "DialogInput" "Ebrima" "Edwardian Script ITC" "Elephant" "Engravers MT" "Eras Bold ITC" "Eras Demi ITC" "Eras Light ITC" "Eras Medium ITC" "Felix Titling" "Fira Code" "Fira Code Light" "Fira Code Medium" "Fira Code Retina" "Footlight MT Light" "Forte" "Franklin Gothic Book" "Franklin Gothic Demi" "Franklin Gothic Demi Cond" "Franklin Gothic Heavy" "Franklin Gothic Medium" "Franklin Gothic Medium Cond" "Freestyle Script" "French Script MT" "Gabriola" "Gadugi" "Garamond" "Georgia" "Gigi" "Gill Sans MT" "Gill Sans MT Condensed" "Gill Sans MT Ext Condensed Bold" "Gill Sans Ultra Bold" "Gill Sans Ultra Bold Condensed" "Gloucester MT Extra Condensed" "Goudy Old Style" "Goudy Stout" "Haettenschweiler" "Harlow Solid Italic" "Harrington" "High Tower Text" "HoloLens MDL2 Assets" "Impact" "Imprint MT Shadow" "Informal Roman" "Javanese Text" "Jokerman" "Juice ITC" "Kristen ITC" ...]

(with-canvas-> c
  (set-font "copperplate gothic light")
  (set-stroke 3.0)
  (draw-letter \g)
  (cast-rays 0.1))

(show-window {:canvas c})

