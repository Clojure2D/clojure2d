;; Reconstruction filters

(ns clojure2d.pbrt.filters
  (:require [clojure2d.math :as m]
            [clojure2d.math.vector :as v])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- windowed-sinc
  "Windowed sinc function"
  ^double [^double v ^double radius ^double rtau]
  (let [x (m/abs v)]
    (if (> x radius) 0.0
        (* (m/sinc x) (m/sinc (* x rtau))))))

(def default-filters-configuration
  {:mitchell {:radius (Vec2. 2.0 2.0)
              :B (/ 1.0 3.0)
              :C (/ 1.0 3.0)}
   :gaussian {:radius (Vec2. 2.0 2.0)
              :alpha 2.0}
   :box {:radius (Vec2. 0.5 0.5)}
   :triangle {:radius (Vec2. 2.0 2.0)}
   :lanczos-sinc {:radius (Vec2. 4.0 4.0)
                  :tau 3.0}})

(def ^:const ^int default-table-width 16)

(deftype ReconstructionFilter [^Vec2 radius filter])

(defn get-filter-config
  ""
  [p c] (merge c (p default-filters-configuration)))

(defmulti make-filter "Create 2d filter method" (fn [a c] a))

(defmethod make-filter :lanczos-sinc
  [p c]
  (let [{:keys [^Vec2 radius ^double tau]} (get-filter-config p c)
        rtau (/ 1.0 tau)]
    (ReconstructionFilter. radius
                           (fn ^double [^Vec2 p]
                             (* (windowed-sinc (.x p) (.x radius) rtau)
                                (windowed-sinc (.y p) (.y radius) rtau))))))

(defmethod make-filter :triangle
  [p c]
  (let [{:keys [^Vec2 radius]} (get-filter-config p c)]
    (ReconstructionFilter. radius
                           (fn ^double [^Vec2 p]
                             (* (max 0.0 (- (.x radius) (m/abs (.x p))))
                                (max 0.0 (- (.y radius) (m/abs (.y p)))))))))

(defmethod make-filter :box
  [p c] (ReconstructionFilter. (:radius (get-filter-config p c))
                               (fn ^double [_] 1.0)))

(defmethod make-filter :gaussian
  [p c]
  (let [{:keys [^Vec2 radius ^double alpha]} (get-filter-config p c)
        alpha- (- alpha)
        gaussian (fn [^double d ^double expv]
                   (max 0.0 (- (m/exp (* alpha- d d)) expv)))
        expx (gaussian (.x radius) 0.0)
        expy (gaussian (.y radius) 0.0)]
    (ReconstructionFilter. radius
                           (fn ^double [^Vec2 p]
                             (* ^double (gaussian (.x p) expx)
                                ^double (gaussian (.y p) expy))))))

(defmethod make-filter :mitchell
  [p c]
  (let [{:keys [radius ^double B ^double C]} (get-filter-config p c)
        ^Vec2 invradius (v/reciprocal radius)
        c6 (* 6.0 C)
        d16 (/ 1.0 6.0)
        mitchell-1d (fn [^double v]
                      (let [x (m/abs (+ v v))
                            x2 (* x x)]
                        (* d16 ^double (if (> x 1)
                                         (+ (* x2 x (- (- B) c6))
                                            (* x2 (+ (* 6.0 B) (* 30.0 C)))
                                            (* x (- (* -12.0 B) (* 48.0 C)))
                                            (+ (* 8.0 B) (* 24.0 C)))
                                         (+ (* x2 x (- 12.0 (* 9.0 B) c6))
                                            (* x2 (+ -18.0 (* 12.0 B) c6))
                                            (- 6.0 (+ B B)))))))]
    (ReconstructionFilter. radius
                           (fn ^double [^Vec2 p]
                             (* ^double (mitchell-1d (* (.x p) (.x invradius)))
                                ^double (mitchell-1d (* (.y p) (.y invradius))))))))

(defn precompute-filter-table
  ""
  ([^ReconstructionFilter f ^long size]
   (let [s2 (* size size)]
     (double-array s2 (for [x (range size)
                            y (range size)]
                        ((.filter f) (v/div (v/emult
                                             (v/add (Vec2. x y) (Vec2. 0.5 0.5))
                                             (.radius f)) size))))))
  ([f] (precompute-filter-table f default-table-width)))

