(ns examples.NOC.ch07.wolframca-figures-7-1
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^int scl 8)
(def ^:const ^int w 1000)
(def ^:const ^int h 800)

(def ^:const ^int cell-no (/ w scl))
(def ^:const ^int rows (/ h scl))

(def ^:const wrap? false) ;; change to wrap cells

(defn make-rule
  "Create rule table based on CA number."
  [^long id]
  (mapv #(if (zero? (bit-and id (bit-shift-left 1 ^long %))) 0 1) (range 7 -1 -1)))

(defn apply-rule
  "Create rule number from `a`, `b`, `c`, parameters (values 0 or 1) and return result from rule table."
  [rule a b c]
  (let [s (str a b c)
        idx (Integer/parseInt s 2)]
    (rule idx)))

(defn init-cells
  "Create first line with one single seed in the middle"
  [^long size]
  (mapv #(if (== ^long % (m/floor (/ size 2))) 1 0) (range size)))

(defn next-cells
  "Calculate next line based on previous and rule, wrap result."
  [cells rule]
  (let [s (count cells)]
    (mapv #(let [^long v %                 
                 [l r] (if wrap?
                         [(cells (int (m/wrap 0 s (dec v))))
                          (cells (int (m/wrap 0 s (inc v))))]
                         [(if (zero? v) 0 (cells (dec v)))
                          (if (< v (dec s)) (cells (inc v)) 0)])]
             (apply-rule rule l (cells v) r)) (range s))))

(defn draw-cells
  "Draw cells."
  [canvas rule]
  (loop [cells (init-cells cell-no)
         row (int 0)]
    (when (< row rows)

      (dotimes [x cell-no]
        (if (== ^int (cells x) 1)
          (set-color canvas :black)
          (set-color canvas :white))
        (rect canvas (* x scl) (* row scl) scl scl)
        (set-color canvas :black)
        (rect canvas (* x scl) (* row scl) scl scl true))
      
      (recur (next-cells cells rule)
             (inc row)))))

(def canvas (make-canvas w h))
(def window (show-window canvas "Wolframca figures 7_1"))

(defn draw-rule
  ""
  ([]
   (draw-rule (r/irand 256))) 
  ([rule]
   (println (str "Rule: " rule))
   (with-canvas-> canvas
     (draw-cells (make-rule rule)))))

(defmethod mouse-event ["Wolframca figures 7_1" :mouse-clicked] [_ _]
  (draw-rule))

(draw-rule 150)
