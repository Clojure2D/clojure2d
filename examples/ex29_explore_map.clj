(ns examples.ex29-explore-map
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.complex :as c])
  (:import [clojure2d.math.complex Complex]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def canvas (create-canvas 600 600))
(def window (show-window canvas "Explore map" 600 600 25))

(defmethod key-pressed ["Explore map" \space] [_]
  (binding [*jpeg-image-quality* 0.9]
    (save-canvas canvas (str (next-filename "results/ex29/") ".jpg"))))

(defn make-standard-map
  "Standard Map"
  [^double K]
  (fn [^double I ^double Theta]
    (let [^double I' (mod (+ I (* K (m/sin Theta))) m/TWO_PI)
          ^double Theta' (mod (+ Theta I') m/TWO_PI)]
      [I' Theta'])))

(defn make-henon-quadratic-map
  "Henon's quadratic map"
  [^double a]
  (let [sina (m/sin a)
        cosa (m/cos a)]
    (fn [^double x ^double y] 
      (let [x2 (* x x)
            x' (- (* x cosa) (* sina (- y x2)))
            y' (+ (* x sina) (* cosa (- y x2)))]
        [x' y']))))

(defn make-henon-map
  "Henon map"
  [^double alpha ^double beta]
  (fn [^double x ^double y]
    [(+ (- 1.0 (* alpha x x)) y)
     (* beta x)]))

(defn make-gingerbreadman-map
  "Gingerbreadman map"
  [^double a ^double b]
  (fn [^double x ^double y]
    [(+ (- 1.0 (* a y)) (* b (m/abs x)))
     x]))

(defn make-ikeda-map
  "Ikeda map"
  [^double u]
  (fn [^double x ^double y]
    (let [t (- 0.4 (/ 6.0 (inc (+ (* x x) (* y y)))))
          sint (m/sin t)
          cost (m/cos t)]
      [(inc (* u (- (* x cost) (* y sint))))
       (* u (+ (* x sint) (* y cost)))])))

(defn make-exponential-map
  "Exp map"
  [^double rec ^double imc]
  (let [^Complex c (Complex. rec imc)]
    (fn [^double re ^double im]
      (let [^Complex z (Complex. re im)
            ^Complex res (c/mult c (c/exp z))]
        [(.real res) (.imag res)]))))


(defn draw-map-position
  "draw N points from given starting point"
  [canvas initx inity [sminx smaxx sminy smaxy] f]
  
  (loop [[x y] [initx inity]
         count (int 0)]
    (when (< count 2000)
      (let [xx (m/norm x sminx smaxx 0 600)
            yy (m/norm y sminy smaxy 0 600)]

        (point canvas xx yy)
        
        (recur (f x y)
               (unchecked-inc count))))))

(defn draw-map
  "draw map from starting point grid"
  [canvas num [rminx rmaxx rminy rmaxy] scale f]
  (set-background canvas 0 0 0)
  (set-color canvas 220 220 220 20)
  (doseq [x (repeatedly num #(m/drand rminx rmaxx))
          y (repeatedly num #(m/drand rminy rmaxy))]
    (draw-map-position canvas x y scale f)))

(def maps {:standard-map [20 [0.0 m/TWO_PI 0.0 m/TWO_PI] [0.0 m/TWO_PI 0.0 m/TWO_PI]
                          make-standard-map [[-5 5]]]
           :henon-quadratic-map [25 [-2.0 2.0 -2.0 2.0] [-4.0 4.0 -4.0 4.0]
                                 make-henon-quadratic-map [[0.5 m/TWO_PI]]]
           :henon-map [70 [-2.0 2.0 -2.0 2.0] [-5.0 5.0 -5.0 5.0]
                       make-henon-map [[0.2 1.2] [0.5 1.2]]]
           :gingerbreadman-map [70 [-10.0 20.0 -10.0 20.0] [-20.0 20.0 -20.0 20.0]
                                make-gingerbreadman-map [[0.5 1.1] [0.5 1.5]]]
           :ikeda-map [70 [-5.0 10.0 -5.0 10.0] [-5.0 10.0 -5.0 10.0]
                       make-ikeda-map [[0.8 0.9999]]]
           :exponential-map [100 [-2.0 2.0 -2.0 2.0] [-2.0 2.0 -2.0 2.0]
                             make-exponential-map [[-1.2 1.2] [-1.2 1.2]]]})

(let [random-map (rand-nth (keys maps))
      [num range scale f pars] (random-map maps)
      rpars (map #(apply m/drand %) pars)]
  (println random-map)
  (println rpars)
  (with-canvas canvas
    (draw-map num range scale (apply f rpars)))
  :done)

