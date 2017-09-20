;; 3d space transformations, pbrt.org

(ns clojure2d.pbrt.transform
  (:require [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [clojure2d.pbrt.matrix :as mat])
  (:import [clojure2d.pbrt.matrix Matrix4x4]
           [clojure2d.math.vector Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defprotocol TransformProto
  (mult [t1 t2]))

(deftype Transform [^Matrix4x4 m ^Matrix4x4 minv]
  TransformProto
  (mult [_ t2]
    (let [^Transform t2 t2]
      (Transform. (mat/mult m (.m t2))
                  (mat/mult (.minv t2) minv)))))

(defmulti make-transform
  "Create various transformations" (fn [a & b] a))

(defmethod make-transform :default
  ([_ m minv] (Transform. m minv))
  ([_ ^Matrix4x4 m] (Transform. m (mat/inverse m)))
  ([_] (Transform. mat/I mat/I)))

(defmethod make-transform :translate
  ([_ ^Vec3 v]
   (Transform. (Matrix4x4. 1.0 0.0 0.0 (.x v)
                           0.0 1.0 0.0 (.y v)
                           0.0 0.0 1.0 (.z v)
                           0.0 0.0 0.0 1.0)
               (Matrix4x4. 1.0 0.0 0.0 (- (.x v))
                           0.0 1.0 0.0 (- (.y v))
                           0.0 0.0 1.0 (- (.z v))
                           0.0 0.0 0.0 1.0)))
  ([_ ^double x ^double y ^double z]
   (Transform. (Matrix4x4. 1.0 0.0 0.0 x
                           0.0 1.0 0.0 y
                           0.0 0.0 1.0 z
                           0.0 0.0 0.0 1.0)
               (Matrix4x4. 1.0 0.0 0.0 (- x)
                           0.0 1.0 0.0 (- y)
                           0.0 0.0 1.0 (- z)
                           0.0 0.0 0.0 1.0))))

(defmethod make-transform :scale
  ([_ ^Vec3 v]
   (Transform. (Matrix4x4. (.x v) 0.0 0.0 0.0
                           0.0 (.y v) 0.0 0.0
                           0.0 0.0 (.z v) 0.0
                           0.0 0.0 0.0 1.0)
               (Matrix4x4. (/ 1.0 (.x v)) 0.0 0.0 0.0
                           0.0 (/ 1.0 (.y v)) 0.0 0.0
                           0.0 0.0 (/ 1.0 (.z v)) 0.0
                           0.0 0.0 0.0 1.0)))
  ([_ ^double x ^double y ^double z]
   (Transform. (Matrix4x4. x 0.0 0.0 0.0
                           0.0 y 0.0 0.0
                           0.0 0.0 z 0.0
                           0.0 0.0 0.0 1.0)
               (Matrix4x4. (/ 1.0 x) 0.0 0.0 0.0
                           0.0 (/ 1.0 y) 0.0 0.0
                           0.0 0.0 (/ 1.0 z) 0.0
                           0.0 0.0 0.0 1.0))))

(defmethod make-transform :rotate
  ([_ ^Vec3 v]
   (make-transform :rotate (.x v) (.y v) (.z v)))
  ([_ ^double x ^double y ^double z]
   (let [result (if (zero? x) mat/I
                    (let [sa (m/sin x)
                          ca (m/cos x)]
                      (Matrix4x4. 1.0 0.0 0.0 0.0
                                  0.0 ca (- sa) 0.0
                                  0.0 sa ca 0.0
                                  0.0 0.0 0.0 1.0)))
         result (if (zero? y) result
                    (let [sa (m/sin y)
                          ca (m/cos y)]
                      (mat/mult result (Matrix4x4. ca 0.0 sa 0.0
                                                   0.0 1.0 0.0 0.0
                                                   (- sa) 0.0 ca 0.0
                                                   0.0 0.0 0.0 1.0))))
         result (if (zero? z) result
                    (let [sa (m/sin z)
                          ca (m/cos z)]
                      (mat/mult result (Matrix4x4. ca (- sa) 0.0 0.0
                                                   sa ca 0.0 0.0
                                                   0.0 0.0 1.0 0.0
                                                   0.0 0.0 0.0 1.0))))]
     (Transform. result (mat/transpose result)))))

(defmethod make-transform :axis-rotate
  ([_ ^double angle ^Vec3 axis]
   (let [^Vec3 a (v/normalize axis)
         sa (m/sin angle)
         ca (m/cos angle)
         ca- (- 1.0 ca)
         x2 (* (.x a) (.x a))
         y2 (* (.y a) (.y a))
         z2 (* (.z a) (.z a))
         xy (* (.x a) (.y a))
         yz (* (.y a) (.z a))
         zx (* (.z a) (.x a))
         m (Matrix4x4. (+ x2 (* (- 1.0 x2) ca))
                       (- (* xy ca-) (* (.z a) sa))
                       (+ (* zx ca-) (* (.y a) sa))
                       0.0
                       (+ (* xy ca-) (* (.z a) sa))
                       (+ y2 (* (- 1.0 y2) ca)) 
                       (- (* yz ca-) (* (.x a) sa))
                       0.0
                       (- (* zx ca-) (* (.y a) sa)) 
                       (+ (* yz ca-) (* (.x a) sa))
                       (+ z2 (* (- 1.0 z2) ca)) 
                       0.0
                       0.0 0.0 0.0 1.0)]
     (Transform. m (mat/transpose m)))))

(defmethod make-transform :orthographic
  [_ ^double near ^double far]
  (mult (make-transform :scale 1.0 1.0 (/ 1.0 (- far near)))
        (make-transform :translate 0.0 0.0 (- near))))

(defmethod make-transform :perspective
  [_ ^double fov ^double n ^double f]
  (let [dfn (- f n)
        persp (Matrix4x4. 1.0 0.0 0.0 0.0
                          0.0 1.0 0.0 0.0
                          0.0 0.0 (/ f dfn) (/ (* (- f) n) dfn)
                          0.0 0.0 1.0 0.0)
        invtanang (/ 1.0 (m/tan (* 0.5 fov)))]
    (mult (make-transform :scale invtanang invtanang 1.0)
          (make-transform :default persp))))

(defmethod make-transform :look-at
  [_ ^Vec3 pos ^Vec3 look ^Vec3 up]
  (let [^Vec3 dir (v/normalize (v/sub look pos))
        ^Vec3 left (v/normalize (v/cross (v/normalize up) dir))
        ^Vec3 newup (v/cross dir left)
        ^Matrix4x4 m (Matrix4x4. (.x left) (.x newup) (.x dir) (.x pos)
                                 (.y left) (.y newup) (.y dir) (.y pos)
                                 (.z left) (.z newup) (.z dir) (.z pos)
                                 0.0 0.0 0.0 1.0)]
    (Transform. (mat/inverse m) m)))

(defn transform-point
  "Transform Vec3 as point"
  [^Transform t ^Vec3 v]
  (let [^Matrix4x4 m (.m t)
        xp (+ (* (.m00 m) (.x v)) (* (.m01 m) (.y v)) (* (.m02 m) (.z v)) (.m03 m))
        yp (+ (* (.m10 m) (.x v)) (* (.m11 m) (.y v)) (* (.m12 m) (.z v)) (.m13 m))
        zp (+ (* (.m20 m) (.x v)) (* (.m21 m) (.y v)) (* (.m22 m) (.z v)) (.m23 m))
        wp (+ (* (.m30 m) (.x v)) (* (.m31 m) (.y v)) (* (.m32 m) (.z v)) (.m33 m))]
    (if (== 1.0 wp)
      (Vec3. xp yp zp)
      (v/div (Vec3. xp yp zp) wp))))

(defn transform-vector
  "Transform Vec3 as vector"
  [^Transform t ^Vec3 v]
  (let [^Matrix4x4 m (.m t)
        xp (+ (* (.m00 m) (.x v)) (* (.m01 m) (.y v)) (* (.m02 m) (.z v)))
        yp (+ (* (.m10 m) (.x v)) (* (.m11 m) (.y v)) (* (.m12 m) (.z v)))
        zp (+ (* (.m20 m) (.x v)) (* (.m21 m) (.y v)) (* (.m22 m) (.z v)))]
    (Vec3. xp yp zp)))

(defn transform-normal
  "Transform Vec3 as normal"
  [^Transform t ^Vec3 v]
  (let [^Matrix4x4 m (.minv t)
        xp (+ (* (.m00 m) (.x v)) (* (.m10 m) (.y v)) (* (.m20 m) (.z v)))
        yp (+ (* (.m01 m) (.x v)) (* (.m11 m) (.y v)) (* (.m21 m) (.z v)))
        zp (+ (* (.m02 m) (.x v)) (* (.m12 m) (.y v)) (* (.m22 m) (.z v)))]
    (Vec3. xp yp zp)))
