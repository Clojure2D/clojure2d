;; http://www.brainfillingcurves.com/

(ns examples.ex37-fractal-bestiary
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [clojure2d.math.random :as r])
  (:import [clojure2d.math.vector Vec2]))

(def ^:const ^int size 800)
(def ^:const ^double s60 (m/sin (m/radians 60.0)))
(def ^:const ^double s60- (- s60))
(def ^:const unit (Vec2. 1.0 0.0))

(def canvas (make-canvas size size))
(def window (show-window canvas "Fractal Bestiary"))

(defn transform-sq->tri
  ""
  [v]
  (let [matched (first (filter #(< (v/angle-between v (first %)) 1.0e-6) [[(Vec2. 1.0 0.0) (Vec2. 1.0 0.0)]
                                                                          [(Vec2. -1.0 0.0) (Vec2. -1.0 0.0)]
                                                                          [(Vec2. 1.0 -1.0) (Vec2. 0.5 s60-)]
                                                                          [(Vec2. 0.0 -1.0) (Vec2. -0.5 s60-)]
                                                                          [(Vec2. -1.0 -1.0) (Vec2. -0.5 s60-)]
                                                                          [(Vec2. -1.0 1.0) (Vec2. -0.5 s60)]
                                                                          [(Vec2. 0.0 1.0) (Vec2. 0.5 s60)]
                                                                          [(Vec2. 1.0 1.0) (Vec2. 0.5 s60)]]))] 
    (if-not matched
      v
      (let [[in tg] matched]
        (v/mult tg (/ (v/mag v) (v/mag in)))))))

(defn process-recipe
  "Process recipe from book format to angles and lengths required by method provided in this example"
  [{:keys [grid recipe pos len]
    :or {pos [0.1 0.6] len 0.25 grid :tri recipe [[1 0 1 1] [0 1 1 1] [1 -1 1 1] [1 0 1 1]]}}]
  (let [transform (if (= grid :tri) transform-sq->tri identity)
        vects (map #(let [[x y] %] (transform (Vec2. x y))) recipe)
        angles (map #(v/relative-angle-between %2 %1) (cons unit vects) vects)
        rot (reduce v/add vects)]
    {:pos pos
     :len len 
     :scale (/ (v/mag rot))
     :rot (v/relative-angle-between unit rot)
     :recipe (map #(conj (drop 2 %2) %1 (v/mag %3)) angles recipe vects)}))

(defn draw-beast
  "Draw recursively given recipe (internally processed)."
  [canvas depth len {:keys [scale rot recipe] :as all}]

  (mapv #(let [[vlen angle forward flip] %
               slen (* vlen len)]
           
           (rotate canvas angle)

           (push-matrix canvas)
           (when (neg? forward)
             (translate canvas slen 0)
             (flip-y canvas)
             (rotate canvas m/PI))
           (when (neg? flip) (flip-y canvas))

           (if (zero? depth)
             (line canvas 0 0 slen 0)
             (do
               (rotate canvas rot)
               (draw-beast canvas (dec depth) (* slen scale) all)))
           (pop-matrix canvas)
           
           (translate canvas slen 0)) recipe)
  canvas)

(defn draw-recipe
  ""
  ([recipe depth]
   (let [{len :len pos :pos :as precipe} (process-recipe recipe)
         [px py] (v/mult pos size)]
     (with-canvas canvas
       (set-background 21 20 25)
       (set-color :lightgrey 250)
       (set-stroke 0.8)
       (translate px py)
       (draw-beast depth (* size len) precipe))))
  ([recipe] (draw-recipe recipe (or (:default-depth recipe) 5))))


(def recipes {;; preliminary part
              :koch {:grid :tri :recipe [[1 0 1 1] [0 1 1 1] [1 -1 1 1] [1 0 1 1]]}
              :p20 {:grid :tri :recipe [[1 0 1 1] [0 1 1 1] [1 -1 1 1] [1 0 1 -1]]}
              :p22 {:grid :sq :recipe [[0 2 1 1] [1 1 1 1] [2 0 1 1] [1 -1 1 1] [0 -2 1 1]] :len 0.1 :pos [0.3 0.7]}
              :p23 {:grid :sq :recipe [[0 2 1 1] [1 1 1 -1] [2 0 1 1] [1 -1 1 -1] [0 -2 1 1]] :len 0.1 :pos [0.3 0.7]}
              :p24 {:grid :sq :recipe [[0 1 1 1] [1 -1 1 1] [1 0 -1 -1]] :pos [0.3 0.6] :default-depth 8}
              ;; :p25 {:grid :sq :recipe [[0 2 -1 1] [1 0 -1 -1] [1 0 1 1] [-1 -1 1 1] [2 -1 1 -1]] :len 0.2 :pos [0.3 0.7]} ;;wrong
              :p37-dragon-curve {:grid :sq :recipe [[1 0 1 1] [0 1 -1 -1]] :len 0.3 :pos [0.3 0.6] :default-depth 11}
              :p37-5-dragon {:grid :sq :recipe [[0 1 1 1] [1 0 1 1] [0 -1 1 1] [1 0 1 1] [0 1 1 1]] :len 0.3 :pos [0.2 0.65]}
              :p50-ab12 {:grid :sq :recipe [[1 0 1 1] [0 1 1 1]] :pos [0.3 0.55] :default-depth 11}
              :p50-a3 {:grid :sq :recipe [[1 0 1 1] [0 1 1 -1]] :pos [0.3 0.55] :len 0.4 :default-depth 11}
              :p50-c1 {:grid :sq :recipe [[1 0 1 -1] [0 1 1 1]] :pos [0.1 0.7] :len 0.4 :default-depth 11}
              :p50-a1 {:grid :sq :recipe [[1 0 1 1] [0 1 -1 -1]] :len 0.4 :pos [0.3 0.6] :default-depth 11}
              :p50-d1 {:grid :sq :recipe [[1 0 -1 -1] [0 1 1 1]] :len 0.4 :pos [0.2 0.7] :default-depth 11}
              :p50-d2 {:grid :sq :recipe [[1 0 -1 -1] [0 1 -1 1]] :len 0.4 :pos [0.15 0.65] :default-depth 13}
              :p50-b4 {:grid :sq :recipe [[1 0 -1 1] [0 1 -1 -1]] :len 0.4 :pos [0.2 0.55] :default-depth 13}
              :p50-c2 {:grid :sq :recipe [[1 0 1 -1] [0 1 -1 1]] :len 0.4 :pos [0.1 0.7] :default-depth 11}
              :p50-b3 {:grid :sq :recipe [[1 0 -1 1] [0 1 1 -1]] :len 0.4 :pos [0.2 0.5] :default-depth 11}
              :p50-cd34 {:grid :sq :recipe [[1 0 -1 -1] [0 1 -1 -1]] :len 0.6 :pos [0.2 0.75] :default-depth 12}

              ;; sqrt(2)
              :p52-dragon-curve {:grid :sq :recipe [[1 0 1 1] [0 1 -1 -1]] :len 0.3 :pos [0.3 0.6] :default-depth 8}
              :p52-polya {:grid :sq :recipe [[1 0 1 -1] [0 1 -1 1]] :len 0.4 :pos [0.1 0.7] :default-depth 8}

              ;; sqrt(3)
              :p53-sq {:grid :sq :recipe [[1 0 1 1] [0 1 1 1] [1 -1 1 1] [1 0 1 1]] :default-depth 3}
              :p53-tri {:grid :tri :recipe [[1 0 1 1] [0 1 1 1] [1 -1 1 1] [1 0 1 1]] :default-depth 3}
              :p55-ter-dragon {:grid :tri :recipe [[0 1 1 1] [1 -1 1 1] [0 1 1 1]] :len 0.4 :pos [0.2 0.7]}
              :p56-inverted-ter-dragon {:grid :tri :recipe [[0 1 -1 1] [1 -1 -1 1] [0 1 -1 1]] :len 0.4 :pos [0.2 0.7]}
              :p56 {:grid :tri :recipe [[0 1 -1 1] [1 -1 1 1] [0 1 -1 1]] :len 0.4 :pos [0.2 0.7]}
              :p57-a {:grid :tri :recipe [[0 1 -1 1] [0 1 -1 1] [1 -1 1 -1]] :len 0.3 :pos [0.35 0.8] :default-depth 6}
              :p57-b {:grid :tri :recipe [[0 1 1 1] [0 1 1 1] [1 -1 -1 -1]] :len 0.3 :pos [0.35 0.8]}
              :p57-c {:grid :tri :recipe [[0 1 1 -1] [0 1 -1 1] [1 -1 1 -1]] :len 0.3 :pos [0.3 0.8] :default-depth 6}
              :p58-a {:grid :tri :recipe [[0 1 1 -1] [0 1 -1 1] [1 -1 -1 1]] :len 0.3 :pos [0.25 0.85] :default-depth 6}
              :p58-b {:grid :tri :recipe [[0 1 -1 -1] [0 1 1 1] [1 -1 1 1]] :len 0.25 :pos [0.25 0.7] :default-depth 6}
              :p58-c {:grid :tri :recipe [[0 1 1 -1] [0 1 -1 1] [1 -1 1 1]] :len 0.25 :pos [0.25 0.7] :default-depth 6}
              :p59-yin-dragon {:grid :tri :recipe [[0 1 1 1] [0 1 -1 -1] [1 -1 1 1]] :len 0.25 :pos [0.25 0.75] :default-depth 6}

              ;; sqrt(4)
              :p60-cesaros-sweep {:grid :sq :recipe [[1 0 1 1] [0 1 1 1] [0 -1 1 1] [1 0 1 1]] :len 0.4 :pos [0.1 0.7] :default-depth 4}
              :p62 {:grid :sq :recipe [[0 1 -1 1] [1 0 1 1] [0 -1 -1 -1] [1 0 1 -1]] :pos [0.35 0.6] :default-depth 4}
              :p63-a {:grid :sq :recipe [[0 1 1 -1] [1 0 -1 1] [0 -1 1 -1] [1 0 -1 1]] :pos [0.25 0.75] :default-depth 4}
              :p63-b {:grid :sq :recipe [[0 1 -1 1] [1 0 1 -1] [0 -1 -1 1] [1 0 1 -1]] :pos [0.3 0.6] :default-depth 4}
              :p64-peano-sweep {:grid :sq :recipe [[0 1 -1 -1] [1 0 1 1] [1 0 1 1] [0 -1 -1 -1]] :len 0.4 :pos [0.1 0.9] :default-depth 4}
              :p66-v1-dragon {:grid :sq :recipe [[1 1 1 1] [1 0 -1 -1] [0 -1 1 1]] :len 0.2 :pos [0.3 0.6] :default-depth 6}
              :p67 {:grid :sq :recipe [[1 1 -1 -1] [1 0 1 1] [0 -1 1 1]] :len 0.2 :pos [0.25 0.6] :default-depth 6}
              :p70 {:grid :sq :recipe [[1 1 -1 1] [0 -1 -1 -1] [1 0 1 1]] :len 0.4 :pos [0.1 0.85] :default-depth 4}
              :p70-v2-dragon {:grid :sq :recipe [[1 1 1 1] [0 -1 1 1] [1 0 -1 -1]] :pos [0.3 0.6] :default-depth 4}
              :p71 {:grid :sq :recipe [[0 1 1 1] [1 -1 -1 -1] [1 0 1 1]] :len 0.3 :pos [0.3 0.6]}
              :p73-dragon-of-eve {:grid :sq :recipe [[0 1 1 1] [1 -1 1 1] [1 0 -1 -1]] :pos [0.3 0.6]}
              :p74-sierpinski-arrowhead-curve {:grid :tri :recipe [[0 1 1 -1] [1 0 1 1] [1 -1 1 -1]] :len 0.4 :pos [0.1 0.85] :default-depth 6}
              :p75-sierpinski-family {:grid :tri :recipe [[0 1 1 -1] [1 -1 -1 -1] [1 0 1 1]] :len 0.4 :pos [0.1 0.7]}
              :p76 {:grid :tri :recipe [[1 -1 1 1] [0 1 1 1] [0 1 1 1] [1 -1 1 1]] :pos [0.25 0.5] :default-depth 4}
              :p77 {:grid :tri :recipe [[1 -1 1 -1] [0 1 -1 1] [0 1 1 -1] [1 -1 1 -1]] :pos [0.25 0.5] :default-depth 3}
              :p78-a {:grid :tri :recipe [[1 -1 -1 1] [0 1 1 1] [0 1 -1 1] [1 -1 -1 1]] :pos [0.25 0.5] :default-depth 4}
              :p78-b {:grid :tri :recipe [[0 1 -1 1] [0 1 1 -1] [1 -1 -1 1] [1 -1 1 -1]] :pos [0.35 0.7]}
              :p80 {:grid :tri :recipe [[0 1 1 1] [0 1 -1 1] [1 -1 1 -1] [1 -1 -1 1]] :len 0.2 :pos [0.3 0.7] :default-depth 4} ;; result different...
              :p81 {:grid :tri :recipe [[0 1 -1 -1] [0 1 1 1] [1 -1 -1 -1] [1 -1 1 1]] :len 0.2 :pos [0.3 0.7] :default-depth 3}
              :p82 {:grid :tri :recipe [[1 0 1 1] [-1 1 -1 -1] [1 0 1 1] [1 -1 -1 -1]] :len 0.4 :pos [0.1 0.85] :default-depth 3}
              :p83-a {:grid :tri :recipe [[1 0 -1 1] [-1 1 1 -1] [1 0 -1 1] [1 -1 1 -1]] :len 0.4 :pos [0.1 0.85] :default-depth 3}
              :p83-b {:grid :tri :recipe [[1 0 1 1] [-1 1 -1 -1] [1 0 1 1] [1 -1 1 -1]] :len 0.4 :pos [0.1 0.85] :default-depth 3}

              ;; sqrt(5)
              })


(draw-recipe (:p82 recipes) 6)
