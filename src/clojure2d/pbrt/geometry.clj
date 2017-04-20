(ns clojure2d.pbrt.geometry
  (:require [clojure2d.math.vector :as v]
            [clojure2d.math :as m])
  (:import [clojure2d.math.vector Vec2 Vec3]
           [clojure.lang IFn]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defprotocol BoundsProto
  (diagonal [b])
  (area [b])
  (volume [b])
  (lerp [b v])
  (offset [b v])
  (inside [b v])
  (union-vec [b v])
  (union-bound [b1 b2])
  (intersect [b1 b2])
  (overlaps [b1 b2])
  (expand [b1 delta])
  (distance-sq [b v])
  (bounding-sphere [b]))

(deftype Bounds2 [^Vec2 pmin ^Vec2 pmax]
  Object
  (toString [_] (str "<" pmin ", " pmax ">"))
  (equals [_ b]
    (and (instance? Bounds2 b)
         (let [^Bounds2 b b]
           (and (= pmin (.pmin b))
                (= pmax (.pmax b))))))
  BoundsProto
  (diagonal [_] (v/sub pmax pmin))
  (area [b]
    (let [^Vec2 d (diagonal ^Bounds2 b)]
      (* (.x d) (.y d))))
  (lerp [_ v] (v/interpolatev pmin pmax v))
  (offset [_ v]
    (let [^Vec2 o (v/sub v pmin)]
      (Vec2. (if (> (.x pmax) (.x pmin)) (/ (.x o) (- (.x pmax) (.x pmin))) (.x o))
             (if (> (.y pmax) (.y pmin)) (/ (.y o) (- (.y pmax) (.y pmin))) (.y o)))))
  (inside [_ v]
    (let [^Vec2 v v]
      (and (<= (.x pmin) (.x v) (.x pmax))
           (<= (.y pmin) (.y v) (.y pmax)))))
  (union-vec [_ v]
    (Bounds2. (v/emn pmin v) (v/emx pmax v)))
  (union-bound [_ b]
    (let [^Bounds2 b b]
      (Bounds2. (v/emn pmin (.pmin b)) (v/emx pmax (.pmax b)))))
  (intersect [_ b]
    (let [^Bounds2 b b]
      (Bounds2. (v/emx pmin (.pmin b)) (v/emn pmax (.pmax b)))))
  (overlaps [_ b]
    (let [^Bounds2 b b
          x (and (>= (.x pmax) (.x ^Vec2 (.pmin b))) (<= (.x pmin) (.x ^Vec2 (.pmax b))))
          y (and (>= (.y pmax) (.y ^Vec2 (.pmin b))) (<= (.y pmin) (.y ^Vec2 (.pmax b))))]
      (and x y)))
  (expand [_ delta]
    (Bounds2. (v/sub pmin (Vec2. delta delta))
              (v/add pmax (Vec2. delta delta))))
  (distance-sq [_ v]
    (let [^Vec2 v v
          dx (max 0.0 (- (.x pmin) (.x v)) (- (.x v) (.x pmax)))
          dy (max 0.0 (- (.y pmin) (.y v)) (- (.y v) (.y pmax)))]
      (+ (* dx dx) (* dy dy))))
  (bounding-sphere [p]
    (let [^Vec2 center (v/mult (v/add pmin pmax) 0.5)
          radius (if (inside p center) (v/dist center pmax) 0.0)]
      [center radius])))

(defn make-bounds2
  "Create bounds 2d"
  ([^Vec2 p1 ^Vec2 p2]
   (Bounds2. (Vec2. (min (.x p1) (.x p2)) (min (.y p1) (.y p2)))
             (Vec2. (max (.x p1) (.x p2)) (max (.y p1) (.y p2)))))
  ([p] (Bounds2. p p))
  ([] (Bounds2. (Vec2. Double/MAX_VALUE Double/MAX_VALUE)
                (Vec2. Double/MIN_VALUE Double/MIN_VALUE))))

(deftype Bounds3 [^Vec3 pmin ^Vec3 pmax]
  Object
  (toString [_] (str "<" pmin ", " pmax ">"))
  (equals [_ b]
    (and (instance? Bounds3 b)
         (let [^Bounds3 b b]
           (and (= pmin (.pmin b))
                (= pmax (.pmax b))))))
  BoundsProto
  (diagonal [_] (v/sub pmax pmin))
  (area [b]
    (let [^Vec3 d (diagonal ^Bounds3 b)]
      (* 2.0 ^double (v/magsq d))))
  (volume [b]
    (let [^Vec3 d (diagonal ^Bounds3 b)]
      (* (.x d) (.y d) (.z d))))
  (lerp [_ v] (v/interpolatev pmin pmax v))
  (offset [_ v]
    (let [^Vec3 o (v/sub v pmin)]
      (Vec3. (if (> (.x pmax) (.x pmin)) (/ (.x o) (- (.x pmax) (.x pmin))) (.x o))
             (if (> (.y pmax) (.y pmin)) (/ (.y o) (- (.y pmax) (.y pmin))) (.y o))
             (if (> (.z pmax) (.z pmin)) (/ (.z o) (- (.z pmax) (.z pmin))) (.z o)))))
  (inside [_ v]
    (let [^Vec3 v v]
      (and (<= (.x pmin) (.x v) (.x pmax))
           (<= (.y pmin) (.y v) (.y pmax))
           (<= (.z pmin) (.z v) (.z pmax)))))
  (union-vec [_ v]
    (Bounds3. (v/emn pmin v) (v/emx pmax v)))
  (union-bound [_ b]
    (let [^Bounds3 b b]
      (Bounds3. (v/emn pmin (.pmin b)) (v/emx pmax (.pmax b)))))
  (intersect [_ b]
    (let [^Bounds3 b b]
      (Bounds3. (v/emx pmin (.pmin b)) (v/emn pmax (.pmax b)))))
  (overlaps [_ b]
    (let [^Bounds3 b b
          x (and (>= (.x pmax) (.x ^Vec3 (.pmin b))) (<= (.x pmin) (.x ^Vec3 (.pmax b))))
          y (and (>= (.y pmax) (.y ^Vec3 (.pmin b))) (<= (.y pmin) (.y ^Vec3 (.pmax b))))
          z (and (>= (.z pmax) (.z ^Vec3 (.pmin b))) (<= (.z pmin) (.z ^Vec3 (.pmax b))))]
      (and x y z)))
  (expand [_ delta]
    (Bounds3. (v/sub pmin (Vec3. delta delta delta))
              (v/add pmax (Vec3. delta delta delta))))
  (distance-sq [_ v]
    (let [^Vec3 v v
          dx (max 0.0 (- (.x pmin) (.x v)) (- (.x v) (.x pmax)))
          dy (max 0.0 (- (.y pmin) (.y v)) (- (.y v) (.y pmax)))
          dz (max 0.0 (- (.z pmin) (.z v)) (- (.z v) (.z pmax)))]
      (+ (* dx dx) (* dy dy) (* dz dz))))
  (bounding-sphere [p]
    (let [^Vec3 center (v/mult (v/add pmin pmax) 0.5)
          radius (if (inside p center) (v/dist center pmax) 0.0)]
      [center radius])))

(defn make-bounds3
  "Create bounds 3d"
  ([^Vec3 p1 ^Vec3 p2]
   (Bounds3. (Vec3. (min (.x p1) (.x p2)) (min (.y p1) (.y p2)) (min (.z p1) (.z p2)))
             (Vec3. (max (.x p1) (.x p2)) (max (.y p1) (.y p2)) (max (.z p1) (.z p2)))))
  ([p] (Bounds3. p p))
  ([] (Bounds3. (Vec3. Double/MAX_VALUE Double/MAX_VALUE Double/MAX_VALUE)
                (Vec3. Double/MIN_VALUE Double/MIN_VALUE Double/MIN_VALUE))))

(defn distance
  "Distance between bound and vector"
  [p v]
  (m/sqrt (distance-sq p v)))

(deftype Ray [^Vec3 o ^Vec3 d ^double tmax medium]
  IFn
  (invoke [_ t]
    (v/add o (v/mult d t))))

(defn make-ray
  "Create ray"
  ([o d tmax medium] (Ray. o d tmax medium))
  ([o d tmax] (Ray. o d tmax nil))
  ([o d] (Ray. o d Double/MAX_VALUE nil)))
