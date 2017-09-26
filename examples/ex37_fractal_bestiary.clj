;; http://www.brainfillingcurves.com/
;;
;; Press keys:
;;
;; * LEFT / RIGHT - change depth
;; * UP / DOWN - zoom in / out
;; * WSAD - move object in window
;; * F - save image
;; * R - random setup
;; * SPACE - randomize preset setup from the book
;; * M - modulate flip (see pages 20-26)
;; * B - draw shorten lines

(ns examples.ex37-fractal-bestiary
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [clojure2d.math.random :as r])
  (:import [clojure2d.math.vector Vec2]
           [java.awt.event KeyEvent]))

(def ^:const ^int size 800)

(def ^:const ^double s60 (m/sin (m/radians 60.0)))
(def ^:const ^double s60- (- s60))
(def ^:const unit (Vec2. 1.0 0.0))

;; canvas

(def canvas (make-canvas size size))

;; main algorithm

(def default-recipe {:pos [0.1 0.6] :len 0.25 :grid :tri :short-line false :depth 5
                     :recipe [[1 0 1 1] [0 1 1 1] [1 -1 1 1] [1 0 1 1]]})

(defn transform-sq->tri
  "Transform square grid coordinates to triangle grid."
  [v]
  (let [matched (first (filter #(< (v/angle-between v (first %)) 1.0e-6) [[(Vec2. 1.0 0.0) (Vec2. 1.0 0.0)]
                                                                          [(Vec2. -1.0 0.0) (Vec2. -1.0 0.0)]
                                                                          [(Vec2. 1.0 -1.0) (Vec2. 0.5 s60-)]
                                                                          [(Vec2. 0.0 -1.0) (Vec2. -0.5 s60-)]
                                                                          [(Vec2. -1.0 -1.0) (Vec2. -0.5 s60-)]
                                                                          [(Vec2. -1.0 1.0) (Vec2. -0.5 s60)]
                                                                          [(Vec2. 0.0 1.0) (Vec2. 0.5 s60)]
                                                                          [(Vec2. 1.0 1.0) (Vec2. 0.5 s60)]]))] 
    (when-let [[in tg] matched] 
      (v/mult tg (/ (v/mag v) (v/mag in))))))

(defn process-recipe
  "Process recipe from book format to angles and lengths required by method provided in this example"
  [recipe]
  (let [{:keys [grid recipe pos len short-line]} (merge default-recipe recipe)
        transform (if (= grid :tri) transform-sq->tri identity)
        vects (map #(let [[x y] %] (transform (Vec2. x y))) recipe)
        angles (map #(v/relative-angle-between %2 %1) (cons unit vects) vects)
        rot (reduce v/add vects)]
    {:pos pos
     :len len
     :short-line short-line
     :scale (/ (v/mag rot))
     :rot (v/relative-angle-between unit rot)
     :recipe (map #(conj (drop 2 %2) %1 (v/mag %3)) angles recipe vects)}))

(defn draw-beast
  "Draw recursively given recipe (internally processed)."
  [canvas depth len {:keys [short-line scale rot recipe] :as all}]

  (run! #(let [[vlen angle forward flip] %
               slen (* vlen len)
               linelen (if short-line (* 0.6 slen) slen)]
           
           (rotate canvas angle)

           (push-matrix canvas)
           (when (neg? forward)
             (-> canvas
                 (translate slen 0)
                 (flip-y)
                 (rotate m/PI)))
           (when (neg? flip) (flip-y canvas))

           (if (zero? depth)
             (line canvas 0 0 linelen 0)
             (-> canvas
                 (rotate rot)
                 (draw-beast (dec depth) (* slen scale) all)))
           
           (pop-matrix canvas)
           
           (translate canvas slen 0)) recipe)
  canvas)

(defn draw-recipe
  "Draw recipe for given depth."
  ([recipe depth]
   (println recipe)
   (let [{len :len pos :pos :as precipe} (process-recipe recipe)
         [px py] (v/mult pos size)]
     (with-canvas canvas
       (set-background 21 20 25)
       (set-color :lightgrey 240)
       (set-stroke 0.8)
       (translate px py)
       (draw-beast depth (* size len) precipe)))
   recipe)
  ([recipe] (draw-recipe recipe (or (:depth recipe) 5))))

;; recipes from book
(def recipes {;; preliminary part
              :koch {:grid :tri :recipe [[1 0 1 1] [0 1 1 1] [1 -1 1 1] [1 0 1 1]]}
              :p20 {:grid :tri :recipe [[1 0 1 1] [0 1 1 1] [1 -1 1 1] [1 0 1 -1]]}
              :p22 {:grid :sq :recipe [[0 2 1 1] [1 1 1 1] [2 0 1 1] [1 -1 1 1] [0 -2 1 1]] :len 0.1 :pos [0.3 0.7]}
              :p23 {:grid :sq :recipe [[0 2 1 1] [1 1 1 -1] [2 0 1 1] [1 -1 1 -1] [0 -2 1 1]] :len 0.1 :pos [0.3 0.7]}
              :p24 {:grid :sq :recipe [[0 1 1 1] [1 -1 1 1] [1 0 -1 -1]] :pos [0.3 0.6] :depth 8}
              ;; :p25 {:grid :sq :recipe [[0 2 -1 1] [1 0 -1 -1] [1 0 1 1] [-1 -1 1 1] [2 -1 1 -1]] :len 0.2 :pos [0.3 0.7]} ;;wrong
              :p37-dragon-curve {:grid :sq :recipe [[1 0 1 1] [0 1 -1 -1]] :len 0.3 :pos [0.3 0.6] :depth 11}
              :p37-5-dragon {:grid :sq :recipe [[0 1 1 1] [1 0 1 1] [0 -1 1 1] [1 0 1 1] [0 1 1 1]] :len 0.3 :pos [0.2 0.65]}
              :p50-ab12 {:grid :sq :recipe [[1 0 1 1] [0 1 1 1]] :pos [0.3 0.55] :depth 11}
              :p50-a3 {:grid :sq :recipe [[1 0 1 1] [0 1 1 -1]] :pos [0.3 0.55] :len 0.4 :depth 11}
              :p50-c1 {:grid :sq :recipe [[1 0 1 -1] [0 1 1 1]] :pos [0.1 0.7] :len 0.4 :depth 11}
              :p50-a1 {:grid :sq :recipe [[1 0 1 1] [0 1 -1 -1]] :len 0.4 :pos [0.3 0.6] :depth 11}
              :p50-d1 {:grid :sq :recipe [[1 0 -1 -1] [0 1 1 1]] :len 0.4 :pos [0.2 0.7] :depth 11}
              :p50-d2 {:grid :sq :recipe [[1 0 -1 -1] [0 1 -1 1]] :len 0.4 :pos [0.15 0.65] :depth 13}
              :p50-b4 {:grid :sq :recipe [[1 0 -1 1] [0 1 -1 -1]] :len 0.4 :pos [0.2 0.55] :depth 13}
              :p50-c2 {:grid :sq :recipe [[1 0 1 -1] [0 1 -1 1]] :len 0.4 :pos [0.1 0.7] :depth 11}
              :p50-b3 {:grid :sq :recipe [[1 0 -1 1] [0 1 1 -1]] :len 0.4 :pos [0.2 0.5] :depth 11}
              :p50-cd34 {:grid :sq :recipe [[1 0 -1 -1] [0 1 -1 -1]] :len 0.6 :pos [0.2 0.75] :depth 12}

              ;; sqrt(2)
              :p52-dragon-curve {:grid :sq :recipe [[1 0 1 1] [0 1 -1 -1]] :len 0.3 :pos [0.3 0.6] :depth 8}
              :p52-polya {:grid :sq :recipe [[1 0 1 -1] [0 1 -1 1]] :len 0.4 :pos [0.1 0.7] :depth 8}

              ;; sqrt(3)
              :p53-sq {:grid :sq :recipe [[1 0 1 1] [0 1 1 1] [1 -1 1 1] [1 0 1 1]] :depth 3}
              :p53-tri {:grid :tri :recipe [[1 0 1 1] [0 1 1 1] [1 -1 1 1] [1 0 1 1]] :depth 3}
              :p55-ter-dragon {:grid :tri :recipe [[0 1 1 1] [1 -1 1 1] [0 1 1 1]] :len 0.4 :pos [0.2 0.7]}
              :p56-inverted-ter-dragon {:grid :tri :recipe [[0 1 -1 1] [1 -1 -1 1] [0 1 -1 1]] :len 0.4 :pos [0.2 0.7]}
              :p56 {:grid :tri :recipe [[0 1 -1 1] [1 -1 1 1] [0 1 -1 1]] :len 0.4 :pos [0.2 0.7]}
              :p57-a {:grid :tri :recipe [[0 1 -1 1] [0 1 -1 1] [1 -1 1 -1]] :len 0.3 :pos [0.35 0.8] :depth 6}
              :p57-b {:grid :tri :recipe [[0 1 1 1] [0 1 1 1] [1 -1 -1 -1]] :len 0.3 :pos [0.35 0.8]}
              :p57-c {:grid :tri :recipe [[0 1 1 -1] [0 1 -1 1] [1 -1 1 -1]] :len 0.3 :pos [0.3 0.8] :depth 6}
              :p58-a {:grid :tri :recipe [[0 1 1 -1] [0 1 -1 1] [1 -1 -1 1]] :len 0.3 :pos [0.25 0.85] :depth 6}
              :p58-b {:grid :tri :recipe [[0 1 -1 -1] [0 1 1 1] [1 -1 1 1]] :len 0.25 :pos [0.25 0.7] :depth 6}
              :p58-c {:grid :tri :recipe [[0 1 1 -1] [0 1 -1 1] [1 -1 1 1]] :len 0.25 :pos [0.25 0.7] :depth 6}
              :p59-yin-dragon {:grid :tri :recipe [[0 1 1 1] [0 1 -1 -1] [1 -1 1 1]] :len 0.25 :pos [0.25 0.75] :depth 6}

              ;; sqrt(4)
              :p60-cesaros-sweep {:grid :sq :recipe [[1 0 1 1] [0 1 1 1] [0 -1 1 1] [1 0 1 1]] :len 0.4 :pos [0.1 0.7] :depth 4}
              :p62 {:grid :sq :recipe [[0 1 -1 1] [1 0 1 1] [0 -1 -1 -1] [1 0 1 -1]] :pos [0.35 0.6] :depth 4}
              :p63-a {:grid :sq :recipe [[0 1 1 -1] [1 0 -1 1] [0 -1 1 -1] [1 0 -1 1]] :pos [0.25 0.75] :depth 4}
              :p63-b {:grid :sq :recipe [[0 1 -1 1] [1 0 1 -1] [0 -1 -1 1] [1 0 1 -1]] :pos [0.3 0.6] :depth 4}
              :p64-peano-sweep {:grid :sq :recipe [[0 1 -1 -1] [1 0 1 1] [1 0 1 1] [0 -1 -1 -1]] :len 0.4 :pos [0.1 0.9] :depth 4}
              :p66-v1-dragon {:grid :sq :recipe [[1 1 1 1] [1 0 -1 -1] [0 -1 1 1]] :len 0.2 :pos [0.3 0.6] :depth 6}
              :p67 {:grid :sq :recipe [[1 1 -1 -1] [1 0 1 1] [0 -1 1 1]] :len 0.2 :pos [0.25 0.6] :depth 6}
              :p70 {:grid :sq :recipe [[1 1 -1 1] [0 -1 -1 -1] [1 0 1 1]] :len 0.4 :pos [0.1 0.85] :depth 4}
              :p70-v2-dragon {:grid :sq :recipe [[1 1 1 1] [0 -1 1 1] [1 0 -1 -1]] :pos [0.3 0.6] :depth 4}
              :p71 {:grid :sq :recipe [[0 1 1 1] [1 -1 -1 -1] [1 0 1 1]] :len 0.3 :pos [0.3 0.6]}
              :p73-dragon-of-eve {:grid :sq :recipe [[0 1 1 1] [1 -1 1 1] [1 0 -1 -1]] :pos [0.3 0.6]}
              :p74-sierpinski-arrowhead-curve {:grid :tri :recipe [[0 1 1 -1] [1 0 1 1] [1 -1 1 -1]] :len 0.4 :pos [0.1 0.85] :depth 6}
              :p75-sierpinski-family {:grid :tri :recipe [[0 1 1 -1] [1 -1 -1 -1] [1 0 1 1]] :len 0.4 :pos [0.1 0.7]}
              :p76 {:grid :tri :recipe [[1 -1 1 1] [0 1 1 1] [0 1 1 1] [1 -1 1 1]] :pos [0.25 0.5] :depth 4}
              :p77 {:grid :tri :recipe [[1 -1 1 -1] [0 1 -1 1] [0 1 1 -1] [1 -1 1 -1]] :pos [0.25 0.5] :depth 3}
              :p78-a {:grid :tri :recipe [[1 -1 -1 1] [0 1 1 1] [0 1 -1 1] [1 -1 -1 1]] :pos [0.25 0.5] :depth 4}
              :p78-b {:grid :tri :recipe [[0 1 -1 1] [0 1 1 -1] [1 -1 -1 1] [1 -1 1 -1]] :pos [0.35 0.7]}
              :p80 {:grid :tri :recipe [[0 1 1 1] [0 1 -1 1] [1 -1 1 -1] [1 -1 -1 1]] :len 0.2 :pos [0.3 0.7] :depth 4} ;; result different...
              :p81 {:grid :tri :recipe [[0 1 -1 -1] [0 1 1 1] [1 -1 -1 -1] [1 -1 1 1]] :len 0.2 :pos [0.3 0.7] :depth 3}
              :p82 {:grid :tri :recipe [[1 0 1 1] [-1 1 -1 -1] [1 0 1 1] [1 -1 -1 -1]] :len 0.4 :pos [0.1 0.85] :depth 3}
              :p83-a {:grid :tri :recipe [[1 0 -1 1] [-1 1 1 -1] [1 0 -1 1] [1 -1 1 -1]] :len 0.4 :pos [0.1 0.85] :depth 3}
              :p83-b {:grid :tri :recipe [[1 0 1 1] [-1 1 -1 -1] [1 0 1 1] [1 -1 1 -1]] :len 0.4 :pos [0.1 0.85] :depth 3}

              ;; sqrt(5)
              :p84-5-dragon {:grid :sq :recipe [[0 1 1 1] [1 0 1 1] [0 -1 1 1] [1 0 1 1] [0 1 1 1]] :pos [0.25 0.65] :depth 3}
              :p84-pinched-5-dragon {:grid :sq :recipe [[0 1 -1 1] [1 0 -1 1] [0 -1 -1 1] [1 0 -1 1] [0 1 -1 1]] :pos [0.25 0.65] :depth 3}
              :p85-quartet {:grid :sq :recipe [[0 1 -1 -1] [0 1 1 1] [1 0 1 1] [0 -1 -1 -1] [1 0 1 1]] :pos [0.35 0.85] :depth 3}
              :p85-inner-flip-quartet {:grid :sq :recipe [[0 1 1 -1] [0 1 -1 1] [1 0 -1 1] [0 -1 1 -1] [1 0 -1 1]] :pos [0.35 0.85] :depth 3}
              :p86-a {:grid :sq :recipe [[0 1 1 -1] [1 0 1 1] [0 1 -1 -1] [1 0 -1 1] [0 -1 -1 -1]] :pos [0.25 0.85] :depth 2}
              :p86-b {:grid :sq :recipe [[0 1 1 -1] [1 0 1 1] [0 1 1 -1] [1 0 -1 1] [0 -1 1 -1]] :pos [0.25 0.85] :depth 2}
              :p87-a {:grid :sq :recipe [[0 1 1 1] [1 0 -1 -1] [0 1 1 1] [1 0 -1 -1] [0 -1 1 1]] :pos [0.25 0.8] :depth 3}
              :p87-b {:grid :sq :recipe [[0 1 -1 1] [1 0 1 -1] [0 1 -1 1] [1 0 1 -1] [0 -1 -1 1]] :pos [0.25 0.75] :depth 3}
              :p87-c {:grid :sq :recipe [[0 1 1 -1] [1 0 -1 1] [0 1 1 -1] [1 0 -1 1] [0 -1 1 -1]] :pos [0.25 0.85] :depth 3}
              :p87-d {:grid :sq :recipe [[0 1 -1 -1] [1 0 1 1] [0 1 -1 -1] [1 0 1 1] [0 -1 -1 -1]] :pos [0.25 0.8] :depth 3}
              :p88-a {:grid :sq :recipe [[0 1 1 -1] [0 1 -1 1] [1 0 1 -1] [1 0 -1 1] [0 -1 1 -1]] :pos [0.3 0.85] :depth 3}
              :p88-b {:grid :sq :recipe [[0 1 1 1] [0 1 -1 -1] [1 0 1 1] [1 0 -1 -1] [0 -1 1 1]] :len 0.2 :pos [0.3 0.8] :depth 3}
              :p89-a {:grid :sq :recipe [[1 0 1 -1] [0 1 1 -1] [0 1 -1 1] [1 0 -1 1] [0 -1 -1 1]] :len 0.2 :pos [0.25 0.7] :depth 3}
              :p89-b {:grid :sq :recipe [[1 0 -1 -1] [0 1 -1 -1] [0 1 1 1] [1 0 1 1] [0 -1 1 1]] :len 0.2 :pos [0.25 0.7] :depth 3}

              ;; sqrt(7)
              :p91-7-dragon {:grid :tri :recipe [[1 0 1 1] [-1 1 1 1] [1 0 1 1] [0 -1 1 1] [1 0 1 1] [-1 1 1 1] [1 0 1 1]] :pos [0.2 0.6] :depth 2}
              :p92 {:grid :tri :recipe [[1 0 -1 1] [1 0 -1 1] [-1 1 1 -1] [0 -1 -1 1] [-1 1 -1 1] [1 0 1 -1] [1 0 1 -1]] :pos [0.2 0.6] :depth 2}
              :p93-a {:grid :tri :recipe [[1 0 1 1] [1 0 -1 -1] [-1 1 -1 -1] [0 -1 -1 -1] [-1 1 1 1] [1 0 -1 -1] [1 0 1 1]] :pos [0.2 0.6] :depth 2}
              :p93-b {:grid :tri :recipe [[1 0 -1 -1] [1 0 -1 -1] [-1 1 1 1] [-1 0 -1 -1] [1 -1 -1 -1] [0 1 1 1] [1 0 -1 -1]] :pos [0.2 0.6] :depth 2}
              :p95-gosper-curve {:grid :tri :recipe [[1 0 1 1] [0 1 -1 -1] [-1 0 -1 -1] [-1 1 1 1] [1 0 1 1] [1 0 1 1] [1 -1 -1 -1]] :pos [0.25 0.8] :depth 2}
              :p96-inner-flip-gosper {:grid :tri :recipe [[1 0 -1 1] [0 1 1 -1] [-1 0 1 -1] [-1 1 -1 1] [1 0 -1 1] [1 0 -1 1] [1 -1 1 -1]] :pos [0.25 0.8] :depth 2}
              :p97-anti-gosper {:grid :tri :recipe [[1 0 1 1] [0 1 1 1] [-1 0 -1 -1] [-1 1 -1 -1] [1 0 1 1] [1 0 -1 -1] [1 -1 -1 -1]] :pos [0.2 0.8] :depth 3}
              :p98 {:grid :tri :recipe [[0 1 -1 -1] [-1 1 -1 -1] [1 0 1 1] [0 -1 1 1] [1 0 -1 -1] [0 1 1 1] [1 -1 -1 -1]] :pos [0.2 0.8] :depth 3}
              :p100 {:grid :tri :recipe [[1 0 1 1] [-1 1 1 1] [1 0 1 1] [1 -1 1 1] [-1 0 1 1] [0 1 1 1] [1 0 1 1]] :depth 2 :pos [0.1 0.65] :len 0.33}
              :p101-root-7-yin {:grid :tri :recipe [[1 0 1 1] [1 0 -1 -1] [-1 1 1 1] [-1 1 -1 -1] [1 0 1 1] [1 0 -1 -1] [0 -1 1 1]] :depth 3 :pos [0.15 0.7] :len 0.23}
              :p102-a {:grid :tri :recipe [[0 -1 1 -1] [1 0 -1 1] [1 0 1 -1] [-1 1 -1 1] [-1 1 1 -1] [1 0 -1 1] [1 0 1 -1]] :depth 3 :pos [0.25 0.5]}
              :p102-b {:grid :tri :recipe [[1 0 1 -1] [1 0 -1 1] [1 0 1 -1] [1 0 -1 1] [-1 1 1 -1] [-1 1 -1 1] [0 -1 1 -1]] :depth 3 :pos [0.05 0.6] :len 0.22}
              :p103 {:grid :tri :recipe [[1 0 1 1] [1 0 -1 -1] [1 0 1 1] [1 0 -1 -1] [-1 1 1 1] [-1 1 -1 -1] [0 -1 1 1]] :depth 3 :pos [0.05 0.6] :len 0.22}
              :p104-a {:grid :tri :recipe [[0 -1 -1 1] [1 0 -1 1] [1 0 1 -1] [1 0 -1 1] [-1 1 -1 1] [-1 1 1 -1] [1 0 1 -1]] :depth 3 :len 0.2 :pos [0.3 0.5]}
              :p104-b {:grid :tri :recipe [[0 -1 1 1] [1 0 1 1] [1 0 -1 -1] [1 0 1 1] [-1 1 1 1] [-1 1 -1 -1] [1 0 -1 -1]] :depth 3 :len 0.2 :pos [0.3 0.5]}
              :p105 {:grid :tri :recipe [[0 1 1 1] [-1 1 1 1] [1 0 1 1] [0 -1 1 1] [1 -1 1 1] [1 0 1 1] [0 1 1 1]] :depth 2 :pos [0.25 0.8]}
              :p106-a {:grid :tri :recipe [[-1 1 -1 -1] [1 0 1 1] [-1 1 -1 -1] [1 0 1 1] [1 0 1 1] [0 -1 -1 -1] [1 0 1 1]] :depth 2 :pos [0.25 0.85]}
              :p106-b {:grid :tri :recipe [[-1 1 1 -1] [1 0 -1 1] [-1 1 1 -1] [1 0 -1 1] [1 0 -1 1] [0 -1 1 -1] [1 0 -1 1]] :depth 2 :pos [0.25 0.85]}
              :p107-a {:grid :tri :recipe [[1 0 1 1] [0 -1 1 1] [1 0 1 1] [-1 1 1 1] [-1 1 1 1] [1 0 1 1] [1 0 1 1]] :depth 2 :pos [0.2 0.5]}
              :p107-b {:grid :tri :recipe [[1 0 -1 -1] [0 -1 -1 -1] [1 0 -1 -1] [-1 1 -1 -1] [-1 1 -1 -1] [1 0 -1 -1] [1 0 -1 -1]] :depth 2 :pos [0.2 0.55]}
              :p108-a {:grid :tri :recipe [[-1 1 -1 1] [1 0 1 -1] [1 0 -1 1] [0 -1 1 -1] [1 0 1 -1] [1 0 1 -1] [-1 1 -1 1]] :depth 2 :len 0.2 :pos [0.25 0.6]}
              :p108-b {:grid :tri :recipe [[-1 1 1 1] [1 0 1 1] [1 0 -1 -1] [0 -1 -1 -1] [1 0 1 1] [1 0 1 1] [-1 1 1 1]] :depth 2 :len 0.2 :pos [0.25 0.6]}
              :p109 {:grid :tri :recipe [[-1 1 1 -1] [-1 1 -1 1] [1 0 1 -1] [1 0 -1 1] [1 0 1 -1] [1 0 -1 1] [0 -1 1 -1]] :depth 3 :len 0.15 :pos [0.4 0.7]}
              :p110 {:grid :tri :recipe [[1 -1 -1 -1] [1 0 1 1] [0 -1 1 1] [-1 1 1 1] [0 1 -1 -1] [1 0 1 1] [0 1 -1 -1]] :depth 2 :pos [0.2 0.45]}

              ;; sqrt(8)
              :p112 {:grid :sq :recipe [[-1 1 -1 1] [0 1 1 -1] [1 1 1 1] [1 0 -1 -1] [1 -1 1 1]] :depth 4 :len 0.165 :pos [0.4 0.75]}
              :p113-a {:grid :sq :recipe [[1 1 1 1] [1 0 1 1] [0 -1 -1 -1] [1 1 1 1] [-1 1 1 -1]] :depth 3 :len 0.23 :pos [0.05 0.7]}
              :p113-b {:grid :sq :recipe [[1 1 1 1] [1 -1 -1 -1] [1 1 1 1] [-1 0 1 -1] [0 1 -1 1]] :depth 3 :len 0.23 :pos [0.05 0.7]}
              :p114 {:grid :sq :recipe [[1 0 1 -1] [0 1 -1 1] [1 -1 -1 -1] [0 2 -1 1]] :len 0.21 :pos [0.1 0.7]}
              :p115 {:grid :sq :recipe [[1 1 1 1] [0 1 1 -1] [-1 0 -1 1] [1 1 1 1] [0 -1 1 -1] [1 0 -1 1]] :depth 2 :pos [0.3 0.9] :len 0.2}
              :p116-a {:grid :sq :recipe [[1 -1 -1 -1] [1 1 1 1] [-1 0 -1 1] [0 1 1 -1] [1 0 -1 1] [0 1 1 -1]] :depth 2 :pos [0.2 0.55] :len 0.2}
              :p116-b {:grid :sq :recipe [[0 2 -1 1] [1 -1 -1 -1] [0 1 -1 1] [1 0 1 -1]] :depth 3 :pos [0.5 0.8] :len 0.2}
              :p117 {:grid :sq :recipe [[-1 1 1 1] [1 1 1 1] [1 -1 1 1] [1 0 -1 -1] [0 1 -1 -1]] :depth 3 :len 0.19 :pos [0.4 0.7]}
              :p118-a {:grid :sq :recipe [[1 -1 1 1] [0 1 -1 -1] [-1 1 1 1] [1 0 -1 -1] [1 1 1 1]] :depth 3 :len 0.25 :pos [0.3 0.6]}
              :p118-b {:grid :sq :recipe [[0 1 -1 -1] [1 0 1 1] [0 -1 -1 -1] [1 0 1 1] [0 -1 -1 -1] [1 0 1 1] [0 -1 -1 -1] [-1 0 1 1]] :depth 2 :len 0.189 :pos [0.2 0.4]}
              :p118-c {:grid :sq :recipe [[0 1 1 1] [0 1 -1 -1] [-1 0 1 1] [0 1 1 1] [1 0 -1 -1] [1 0 1 1] [0 -1 -1 -1] [1 0 -1 -1]] :depth 2 :len 0.189 :pos [0.5 0.85]}
              :p118-d {:grid :sq :recipe [[1 0 1 1] [0 1 -1 -1] [-1 0 1 1] [0 1 -1 -1] [1 0 1 1] [0 -1 -1 -1] [1 0 1 1] [0 1 -1 -1]] :depth 2 :len 0.25 :pos [0.25 0.75]}
              :p118-e {:grid :sq :recipe [[-1 1 -1 -1] [-1 1 1 1] [1 0 -1 -1] [1 0 1 1] [1 0 -1 -1] [1 0 1 1]] :depth 4 :len 0.189 :pos [0.55 0.75]}
              :p119-twin-twin-dragon {:grid :sq :recipe [[0 1 1 1] [0 1 1 1] [1 -1 1 1] [1 -1 -1 -1] [0 1 1 1] [0 1 -1 -1]] :depth 3 :len 0.25 :pos [0.25 0.75]}
              :p120 {:grid :sq :recipe [[0 2 1 1] [1 -1 1 1] [1 0 1 1] [0 1 -1 -1]] :depth 3 :len 0.189 :pos [0.35 0.7]}
              :p121 {:grid :sq :recipe [[-1 1 1 1] [1 0 1 1] [0 1 -1 -1] [1 -1 1 1] [1 0 1 1] [0 1 -1 -1]] :depth 2 :len 0.217 :pos [0.35 0.7]}
              :p122 {:grid :sq :recipe [[1 1 -1 -1] [-1 0 -1 -1] [0 1 1 1] [2 0 1 1]] :depth 3 :len 0.217 :pos [0.3 0.8]}
              :p123 {:grid :sq :recipe [[0 2 1 1] [1 0 1 1] [0 -1 -1 -1] [1 0 1 1] [0 1 -1 -1]] :depth 2 :len 0.217 :pos [0.35 0.75]}
              :p124-curled-dragon-of-eve {:grid :sq :recipe [[-1 1 1 1] [0 1 1 1] [1 -1 1 1] [1 0 -1 -1] [1 1 -1 -1]] :depth 3 :len 0.189 :pos [0.4 0.7]}
              :p125-brainfiller {:grid :sq :recipe [[0 1 -1 1] [1 0 1 -1] [0 -1 -1 1] [1 0 1 -1] [0 2 1 -1]] :depth 3 :len 0.2875 :pos [0.15 0.7]}

              ;; sqrt(9)
              :p126-square-koch {:grid :sq :recipe [[1 0 1 1] [0 1 1 1] [1 0 1 1] [0 -1 1 1] [1 0 1 1]] :depth 3}
              :p126-original-peano-curve {:grid :sq :recipe [[1 0 1 1] [0 1 1 1] [1 0 1 1] [0 -1 1 1] [-1 0 1 1] [0 -1 1 1] [1 0 1 1] [0 1 1 1] [1 0 1 1]] :depth 2 :pos [0.1 0.5]}
              })

;; (draw-recipe (set-state! window (:p126-original-peano-curve recipes)))

;; window / events

(def window-name "Fractal Bestiary - Brainfilling Curves.")
(def window (show-window {:canvas canvas
                          :window-name window-name
                          :state default-recipe}))

(defmethod key-released [window-name (char 0xffff)] [^KeyEvent e {depth :depth len :len :or {depth 5 len 0.25} :as state}]
  (let [ndepth (condp = (.getKeyCode e)
                 KeyEvent/VK_RIGHT (min 20 (inc depth))
                 KeyEvent/VK_LEFT (max 0 (dec depth)) 
                 depth)
        nlen (condp = (.getKeyCode e)
               KeyEvent/VK_UP (* len 1.15)
               KeyEvent/VK_DOWN (/ len 1.15)
               len)]
    (if (or (not= len nlen) (not= ndepth depth))
      (do
        (println (str "New depth: " ndepth)) 
        (draw-recipe (assoc state :depth ndepth :len nlen)))
      state)))

(defmethod key-pressed [window-name \space] [_ _]
  (let [name (rand-nth (keys recipes))]
    (println (str "Recipe: " name))
    (draw-recipe (name recipes))))

(defmethod key-pressed [window-name \b] [_ {short-line :short-line :as state}]
  (draw-recipe (assoc state :short-line (not short-line))))

(defmethod key-pressed [window-name \r] [_ _]
  (draw-recipe {:grid (if (r/brand) :tri :sq)
                :len 0.1
                :pos [0.5 0.5]
                :recipe (filter #(let [[x y] %]
                                   (not (and (zero? x) (zero? y)))) (repeatedly (r/irand 2 9)
                                                                                #(vector (r/irand -1 2)
                                                                                         (r/irand -1 2)
                                                                                         (r/randval -1 1)
                                                                                         (r/randval -1 1))))}))

(defmethod key-pressed [window-name \m] [_ {recipe :recipe :as state}]
  (draw-recipe (assoc state :recipe (map #(let [[x y a b] %]
                                            [x y (r/randval -1 1) (r/randval -1 1)]) recipe))))

(defmethod key-pressed [window-name \w] [_ {pos :pos :or {pos [0.1 0.6]} :as state}]
  (let [[x y] pos] (draw-recipe (assoc state :pos [x (- y 0.05)]))))
(defmethod key-pressed [window-name \s] [_ {pos :pos :or {pos [0.1 0.6]} :as state}]
  (let [[x y] pos] (draw-recipe (assoc state :pos [x (+ y 0.05)]))))
(defmethod key-pressed [window-name \a] [_ {pos :pos :or {pos [0.1 0.6]} :as state}]
  (let [[x y] pos] (draw-recipe (assoc state :pos [(- x 0.05) y]))))
(defmethod key-pressed [window-name \d] [_ {pos :pos :or {pos [0.1 0.6]} :as state}]
  (let [[x y] pos] (draw-recipe (assoc state :pos [(+ x 0.05) y]))))

(defmethod key-pressed [window-name \f] [_ state]
  (save-canvas canvas (next-filename "results/ex37/" ".jpg"))
  state)

(draw-recipe (get-state window))


