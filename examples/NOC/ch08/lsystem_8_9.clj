(ns examples.NOC.ch08.lsystem-8-9
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v]))

(def ^:const ^int s 600)

(def canvas (make-canvas s s))

(defn make-generator
  "Apply rules to the string"
  [rules]
  (fn [^String s]
    (reduce #(str %1 (or (rules %2) %2)) "" s)))

(defn make-turtle
  "Create turtle fn which moves turtle"
  [len theta changelen visible]
  (let [rtheta (m/radians theta)]
    (fn [canvas chr depth]
      (let [nlen (* len (m/pow changelen depth))]
        (condp = chr
          \+ (rotate canvas rtheta)
          \- (rotate canvas (- rtheta))
          \[ (push-matrix canvas)
          \] (pop-matrix canvas)
          (when (visible chr)
            (-> canvas
                (line 0 0 nlen 0)
                (translate nlen 0))))))))

(def rulesets (reduce #(assoc %1 (:rule-name %2) %2) {}
                      [{:rule-name :tree
                        :sentence "F"
                        :turtle (make-turtle (/ s 4.0) 25 0.5 #{\F})
                        :generator (make-generator {\F "FF+[+F-F-F]-[-F+F+F]"})
                        :init-pos (v/vec2 (/ s 2.0) s)}

                       {:rule-name :sierpinski
                        :sentence "F-G-G"
                        :maxdepth 8
                        :init-pos (v/vec2 (* 0.9 s) s)
                        :turtle (make-turtle s 120 0.5 #{\F \G})
                        :generator (make-generator {\F "F-G+F+G-F"
                                                    \G "GG"})}
                       {:rule-name :shrub
                        :sentence "F"
                        :init-pos (v/vec2 (/ s 2.0) s)
                        :turtle (make-turtle (/ s 2.0) 25 0.5 #{\F})
                        :generator (make-generator {\F "F[+F]F[-F][F]"})}

                       {:rule-name :reptile-4
                        :sentence "R"
                        :maxdepth 6
                        :turtle (make-turtle (/ s 2) 60 0.5 #{\F})
                        :init-pos (v/vec2 (/ s 4.0) 0)
                        :generator (make-generator {\F "FF"
                                                    \R "++FFR+FFR+FFR++FFFRF"})}

                       {:rule-name :sierpinski-median-curve
                        :sentence "L--F--L--F"
                        :maxdepth 11
                        :turtle (make-turtle s 45 0.65 #{\F})
                        :init-pos (v/vec2 (/ s 2) (* 0.95 s))
                        :generator (make-generator {\F "F"
                                                    \L "+R-F-R+"
                                                    \R "-L+F+L-"})}

                       {:rule-name :penrose-tiling
                        :sentence "[B]++[B]++[B]++[B]++[B]"
                        :turtle (make-turtle (/ s 2) 36 0.6 #{\A \B \C \D})
                        :init-pos (v/vec2 (/ s 2) (/ s 2))
                        :generator (make-generator {\A "CE++DE----BE[-CE----AE]++"
                                                    \B "+CE--DE[---AE--BE]+"
                                                    \C "-AE++BE[+++CE++DE]-"
                                                    \D "--CE++++AE[+DE++++BE]--BE"
                                                    \E ""})}]))

(def window (show-window canvas "L-System"))

(defn run-turtle
  "Draw sentence"
  [canvas turtle sentence depth]
  (run! #(turtle canvas %1 depth) sentence))

(defn draw-sentence
  "Prepare canvas and run turtle"
  [canvas init-pos turtle sentence depth]
  (with-canvas-> canvas
    (set-background :white)
    (set-color :black) 
    (text "Click mouse to generate. Click space for random change." 10 (- (height canvas) 10)) ;
    (translate init-pos)
    (rotate (- m/HALF_PI))
    (run-turtle turtle sentence depth)))

(defmethod mouse-event [(:window-name window) :mouse-pressed] [_ {:keys [init-pos rule-name maxdepth depth sentence turtle generator] :or {depth 0 maxdepth 5} :as all}]
  (let [ndepth (inc depth)]
    (if (<= ndepth maxdepth) 
      (let [nsentence (generator sentence)]
        (draw-sentence canvas init-pos turtle nsentence ndepth)
        (assoc all :depth ndepth :sentence nsentence))
      (do
        (println (str "Max depth (" maxdepth ") reached."))
        all))))

(defmethod key-pressed [(:window-name window) \space] [_ _]
  (let [r (rulesets (rand-nth (keys rulesets)))
        b (assoc r :sentence ((:generator r) (:sentence r)) :depth 1)]
    (println b)
    (draw-sentence canvas (:init-pos b) (:turtle b) (:sentence b) (:depth b))
    b))

(let [b (set-state! window (:penrose-tiling rulesets))]
  (println b)
  (draw-sentence canvas (:init-pos b) (:turtle b) (:sentence b) 0))

