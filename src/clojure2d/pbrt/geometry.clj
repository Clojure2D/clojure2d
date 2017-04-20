(ns clojure2d.pbrt.geometry
  (:require [clojure2d.math.vector :as v]
            [clojure2d.math :as m])
  (:import [clojure2d.math.vector Vec2 Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defprotocol BoundsProto
  (diagonal [b])
  (area [b])
  (volume [b])
  (lerp [b v]))

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
  (lerp [_ v] (v/interpolatev pmin pmax v)))

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
  (lerp [_ v] (v/interpolatev pmin pmax v)))

(defn make-bounds3
  "Create bounds 3d"
  ([^Vec3 p1 ^Vec3 p2]
   (Bounds3. (Vec3. (min (.x p1) (.x p2)) (min (.y p1) (.y p2)) (min (.z p1) (.z p2)))
             (Vec3. (max (.x p1) (.x p2)) (max (.y p1) (.y p2)) (max (.z p1) (.z p2)))))
  ([p] (Bounds3. p p))
  ([] (Bounds3. (Vec3. Double/MAX_VALUE Double/MAX_VALUE Double/MAX_VALUE)
                (Vec3. Double/MIN_VALUE Double/MIN_VALUE Double/MIN_VALUE))))
