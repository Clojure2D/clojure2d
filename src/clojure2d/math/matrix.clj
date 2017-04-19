;; ### 4x4 Matrix Transformations
;;
;; Straightforward implementation

(ns clojure2d.math.matrix
  (:require [clojure2d.math :as m]
            [clojure2d.math.vector :as v]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defprotocol Matrix4x4Proto
  (transpose [m])
  (inverse [m])
  (mult [m1 m2]))

(deftype Matrix4x4 [^double m00 ^double m01 ^double m02 ^double m03
                    ^double m10 ^double m11 ^double m12 ^double m13
                    ^double m20 ^double m21 ^double m22 ^double m23
                    ^double m30 ^double m31 ^double m32 ^double m33]
  Object
  (toString [_] (str "[" m00 ", " m01 ", " m02 ", " m03 "]\n"
                     "[" m10 ", " m11 ", " m12 ", " m13 "]\n"
                     "[" m20 ", " m21 ", " m22 ", " m23 "]\n"
                     "[" m30 ", " m31 ", " m32 ", " m33 "]\n"))
  (equals [_ v]
    (and (instance? Matrix4x4 v)
         (let [^Matrix4x4 v v]
           (and (== m00 (.m00 v)) (== m01 (.m01 v)) (== m02 (.m02 v)) (== m03 (.m03 v))
                (== m10 (.m10 v)) (== m11 (.m11 v)) (== m12 (.m12 v)) (== m13 (.m13 v))
                (== m20 (.m20 v)) (== m21 (.m21 v)) (== m22 (.m22 v)) (== m23 (.m23 v))
                (== m30 (.m30 v)) (== m31 (.m31 v)) (== m32 (.m32 v)) (== m33 (.m33 v))))))
  Matrix4x4Proto
  (transpose [_] (Matrix4x4. m00 m10 m20 m30
                             m01 m11 m21 m31
                             m02 m12 m22 m23
                             m03 m13 m23 m33))
  (inverse [_]
    (let [s0 (- (* m00 m11) (* m10 m01))
          s1 (- (* m00 m12) (* m10 m02))
          s2 (- (* m00 m13) (* m10 m03))
          s3 (- (* m01 m12) (* m11 m02))
          s4 (- (* m01 m13) (* m11 m03))
          s5 (- (* m02 m13) (* m12 m03))
          c5 (- (* m22 m33) (* m32 m23))
          c4 (- (* m21 m33) (* m31 m23))
          c3 (- (* m21 m32) (* m31 m22))
          c2 (- (* m20 m33) (* m30 m23))
          c1 (- (* m20 m32) (* m30 m22))
          c0 (- (* m20 m31) (* m30 m21))
          det (-> (* s0 c5)
                  (- (* s1 c4))
                  (+ (* s2 c3))
                  (+ (* s3 c2))
                  (- (* s4 c1))
                  (+ (* s5 c0)))]
      (if (zero? det) nil
          (let [invdet (/ 1.0 det)]
            (Matrix4x4. (-> (* m11 c5)
                            (- (* m12 c4))
                            (+ (* m13 c3))
                            (* invdet))
                        (-> (- (* m01 c5))
                            (+ (* m02 c4))
                            (- (* m03 c3))
                            (* invdet))
                        (-> (* m31 s5)
                            (- (* m32 s4))
                            (+ (* m33 s3))
                            (* invdet))
                        (-> (- (* m21 s5))
                            (+ (* m22 s4))
                            (- (* m23 s3))
                            (* invdet))

                        (-> (- (* m10 c5))
                            (+ (* m12 c2))
                            (- (* m13 c1))
                            (* invdet))
                        (-> (* m00 c5)
                            (- (* m02 c2))
                            (+ (* m03 c1))
                            (* invdet))
                        (-> (- (* m30 s5))
                            (+ (* m32 s2))
                            (- (* m33 s1))
                            (* invdet))
                        (-> (* m20 s5)
                            (- (* m22 s2))
                            (+ (* m23 s1))
                            (* invdet))

                        (-> (* m10 c4)
                            (- (* m11 c2))
                            (+ (* m13 c0))
                            (* invdet))
                        (-> (- (* m00 c4))
                            (+ (* m01 c2))
                            (- (* m03 c0))
                            (* invdet))
                        (-> (* m30 s4)
                            (- (* m31 s2))
                            (+ (* m33 s0))
                            (* invdet))
                        (-> (- (* m20 s4))
                            (+ (* m21 s2))
                            (- (* m23 s0))
                            (* invdet))

                        (-> (- (* m10 c3))
                            (+ (* m11 c1))
                            (- (* m12 c0))
                            (* invdet))
                        (-> (* m00 c3)
                            (- (* m01 c1))
                            (+ (* m02 c0))
                            (* invdet))
                        (-> (- (* m30 s3))
                            (+ (* m31 s1))
                            (- (* m32 s0))
                            (* invdet))
                        (-> (* m20 s3)
                            (- (* m21 s1))
                            (+ (* m22 s0))
                            (* invdet)))))))
  (mult [_ m]
    (let [^Matrix4x4 m m]
      (Matrix4x4. (-> (* m00 (.m00 m))
                      (+ (* m01 (.m10 m)))
                      (+ (* m02 (.m20 m)))
                      (+ (* m03 (.m30 m))))
                  (-> (* m00 (.m01 m))
                      (+ (* m01 (.m11 m)))
                      (+ (* m02 (.m21 m)))
                      (+ (* m03 (.m31 m))))
                  (-> (* m00 (.m02 m))
                      (+ (* m01 (.m12 m)))
                      (+ (* m02 (.m22 m)))
                      (+ (* m03 (.m32 m))))
                  (-> (* m00 (.m03 m))
                      (+ (* m01 (.m13 m)))
                      (+ (* m02 (.m23 m)))
                      (+ (* m03 (.m33 m))))

                  (-> (* m10 (.m00 m))
                      (+ (* m11 (.m10 m)))
                      (+ (* m12 (.m20 m)))
                      (+ (* m13 (.m30 m))))
                  (-> (* m10 (.m01 m))
                      (+ (* m11 (.m11 m)))
                      (+ (* m12 (.m21 m)))
                      (+ (* m13 (.m31 m))))
                  (-> (* m10 (.m02 m))
                      (+ (* m11 (.m12 m)))
                      (+ (* m12 (.m22 m)))
                      (+ (* m13 (.m32 m))))
                  (-> (* m10 (.m03 m))
                      (+ (* m11 (.m13 m)))
                      (+ (* m12 (.m23 m)))
                      (+ (* m13 (.m33 m))))

                  (-> (* m20 (.m00 m))
                      (+ (* m21 (.m10 m)))
                      (+ (* m22 (.m20 m)))
                      (+ (* m23 (.m30 m))))
                  (-> (* m20 (.m01 m))
                      (+ (* m21 (.m11 m)))
                      (+ (* m22 (.m21 m)))
                      (+ (* m23 (.m31 m))))
                  (-> (* m20 (.m02 m))
                      (+ (* m21 (.m12 m)))
                      (+ (* m22 (.m22 m)))
                      (+ (* m23 (.m32 m))))
                  (-> (* m20 (.m03 m))
                      (+ (* m21 (.m13 m)))
                      (+ (* m22 (.m23 m)))
                      (+ (* m23 (.m33 m))))

                  (-> (* m30 (.m00 m))
                      (+ (* m31 (.m10 m)))
                      (+ (* m32 (.m20 m)))
                      (+ (* m33 (.m30 m))))
                  (-> (* m30 (.m01 m))
                      (+ (* m31 (.m11 m)))
                      (+ (* m32 (.m21 m)))
                      (+ (* m33 (.m31 m))))
                  (-> (* m30 (.m02 m))
                      (+ (* m31 (.m12 m)))
                      (+ (* m32 (.m22 m)))
                      (+ (* m33 (.m32 m))))
                  (-> (* m30 (.m03 m))
                      (+ (* m31 (.m13 m)))
                      (+ (* m32 (.m23 m)))
                      (+ (* m33 (.m33 m))))))))

(def ^Matrix4x4 I (Matrix4x4. 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1))

(defprotocol TransformProto
  (multiply [t1 t2]))

(deftype Transform [^Matrix4x4 m ^Matrix4x4 invm]
  TransformProto
  (multiply [_ t2]
    (let [^Transform t2 t2]
      (Transform. (mult m (.m t2))
                  (mult (.invm t2) invm)))))

(defn make-transform
  "Create tranformation"
  ([m minv] (Transform. m minv))
  ([^Matrix4x4 m] (Transform. m (inverse m)))
  ([] (Transform. I I)))

(defn make-scale-transform
  "Create scale transform"
  []
  )

(defn make-translate-transform
  ""
  []
  )



(def m (Matrix4x4. 1 1 1 1 0 2 2 2 0 0 3 3 0 0 0 4))
