(ns clojure2d.color.whitepoints
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [fastmath.matrix :as mat]
            [fastmath.interpolation :as i]
            [clojure.java.io :refer [resource input-stream]]
            [clojure.edn :as edn])
  (:import [fastmath.vector Vec2 Vec3 Vec4]
           [fastmath.matrix Mat3x3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn- read-edn [n] (-> (resource n) input-stream slurp edn/read-string))

(defn xy->XZ
  "Convert xy chroma coordinates (from Yxy) to XZ coordinates (from XYZ)."
  (^Vec2 [^Vec2 xy] (xy->XZ (.x xy) (.y xy)))
  (^Vec2 [^double x ^double y]
   (-> (Vec2. x (- 1.0 x y))
       (v/div y))))

(defn xy->whitepoint
  "Convert xy chroma coordinates to XZxy whitepoint definition."
  (^Vec4 [^Vec2 xy] (xy->whitepoint (.x xy) (.y xy)))
  (^Vec4 [^double x ^double y]
   (let [^Vec2 XZ (xy->XZ x y)]
     (Vec4. (.x XZ) (.y XZ) x y))))

(defn XZ->xy
  "Convert XZ coordinates (from XYZ) to xy coordinates (from Yxy). "
  (^Vec2 [^Vec2 XZ] (XZ->xy (.x XZ) (.y XZ)))
  (^Vec2 [^double X ^double Z]
   (-> (Vec2. X 1.0)
       (v/div (+ 1.0 X Z)))))

(defn XZ->whitepoint
  "Convert XZ coordinates to XZxy whitepoint definition."
  (^Vec4 [^Vec2 XZ] (XZ->whitepoint (.x XZ) (.y XZ)))
  (^Vec4 [^double X ^double Z]
   (let [^Vec2 xy (XZ->xy X Z)]
     (Vec4. X Z (.x xy) (.y xy)))))

(defn xy->uv
  (^Vec2 [^Vec2 xy] (xy->uv (.x xy) (.y xy)))
  (^Vec2 [^double x ^double y]
   (let [d (/ (+ (* 12.0 y)
                 (* -2.0 x)
                 3.0))]
     (Vec2. (* 4.0 x d)
            (* 6.0 y d)))))

(defn uv->xy
  (^Vec2 [^Vec2 uv] (uv->xy (.x uv) (.y uv)))
  (^Vec2 [^double u ^double v]
   (let [d (/ (+ (* 2.0 u)
                 (* -8.0 v)
                 4.0))]
     (Vec2. (* 3.0 u d)
            (* 2.0 v d)))))

(defn- add-Y
  (^Vec3 [XZ] (add-Y XZ 1.0))
  (^Vec3 [^Vec2 XZ Y]
   (Vec3. (.x XZ) Y (.y XZ))))

(def whitepoints (read-edn "color/whitepoints.edn"))

(defmacro ^:private gen-whitepoints []
  `(do
     ~@(for [[k [X Z x y]] whitepoints]
         `(def ~(symbol (name k)) (Vec4. ~X ~Z ~x ~y)))))

(gen-whitepoints)

(defn whitepoint
  "Create whitepoint 4 element vector containing X, Z and x,y chroma coordinates"
  (^Vec4 [^double X ^double Z ^double x ^double y] (Vec4. X Z x y))
  (^Vec4 [^double x ^double y] (xy->whitepoint x y)))

(defn tristimulus
  "Create XYZ color for given white point as predifined var or x,y coordinates.

  Default is `CIE-2-D65`."
  ([] (tristimulus CIE-2-D65))
  ([^Vec4 whitepoint]
   (Vec3. (.x whitepoint) 1.0 (.y whitepoint)))
  ([^double x ^double y]
   (-> (Vec2. x y) xy->XZ add-Y)))

;; http://www.brucelindbloom.com/index.html?Eqn_ChromAdapt.html

(def ^:private chromatic-adaptation-methods
  (let [I         (mat/eye 3)
        bradford  (mat/mat3x3 0.8951 0.2664 -0.1614 -0.7502 1.7135 0.0367 0.0389 -0.0685  1.0296)
        von-kries (mat/mat3x3 0.40024 0.7076 -0.08081 -0.2263 1.16532 0.0457 0.0 0.0 0.9182200)
        sharp (mat/mat3x3 1.2694 -0.0988 -0.1706 -0.8364 1.8006 0.0357 0.0297 -0.0315 1.0018)
        fairchild (mat/mat3x3 0.8562 0.3372 -0.1934 -0.8360 1.8327 0.0033 0.0357 -0.0469 1.0112)
        cat97 (mat/mat3x3 0.8951 -0.7502 0.0389 0.2664 1.7135 0.0685 -0.1614 0.0367 1.0296)
        cat2000 (mat/mat3x3  0.7982 0.3389 -0.1371 -0.5918 1.5512 0.0406 0.0008 0.0239 0.9753)
        cat02 (mat/mat3x3  0.7328 0.4296 -0.1624 -0.7036 1.6975 0.0061 0.0030 0.0136 0.9834)
        cat02brill2008 (mat/mat3x3 0.7328 0.4296 -0.1624 -0.7036 1.6975 0.0061 0.0000 0.0000 1.0000)
        cat16 (mat/mat3x3 0.401288 0.650173 -0.051461 -0.250268 1.204414 0.045854 -0.002079 0.048952 0.953127)
        bianco2010 (mat/mat3x3 0.8752 0.2787 -0.1539 -0.8904 1.8709 0.0195 -0.0061 0.0162 0.9899)
        bianco2010-pc (mat/mat3x3 0.6489 0.3915 -0.0404 -0.3775 1.3055 0.0720 -0.0271 0.0888 0.9383)]
    {:xyz-scaling [I I]
     :bradford    [bradford (mat/inverse bradford)]
     :von-kries   [von-kries (mat/inverse von-kries)]
     :sharp       [sharp (mat/inverse sharp)]
     :fairchild   [fairchild (mat/inverse fairchild)]
     :cat97       [cat97 (mat/inverse cat97)]
     :cat2000     [cat2000 (mat/inverse cat2000)]
     :cat02       [cat02 (mat/inverse cat02)]
     :cat02brill2008  [cat02brill2008 (mat/inverse cat02brill2008)]
     :cat16       [cat16 (mat/inverse cat16)]
     :bianco2010  [bianco2010 (mat/inverse bianco2010)]
     :bianco2010-pc [bianco2010-pc (mat/inverse bianco2010-pc)]}))

(defn chromatic-adaptation-matrix
  "Von-Kries convertion between two different white points. Returns forward and inverse conversion 3x3 matrices.

  Adaptation method can be one of: `:xyz-scaling`, `:bradford`, `:von-kries` (default), `:sharp`, `:fairchild`, `:cat97`, `:cat2000`, `:cat02`, `:cat02brill2008`, `:cat16`, `bianco2010`, `:bianco2010-pc`."
  (^Mat3x3 [adaptation-method source-wp destination-wp]
   (let [ws (tristimulus source-wp)
         wd (tristimulus destination-wp)
         [Ma Ma-1] (chromatic-adaptation-methods adaptation-method)
         s (mat/mulv Ma ws)
         d (mat/mulv Ma wd)
         diag (mat/diagonal (v/ediv d s))
         M (mat/mulm Ma-1 (mat/mulm diag Ma))]
     M)))

(def rgbs
  {:sRGB {:red (Vec2. 0.64 0.33) :green (Vec2. 0.3 0.6) :blue (Vec2. 0.15 0.06)}})

;; generate XYZ conversion matrix
(comment (let [r (tristimulus (get-in rgbs [:sRGB :red]))
               g (tristimulus (get-in rgbs [:sRGB :green]))
               b (tristimulus (get-in rgbs [:sRGB :blue]))
               w (tristimulus [:CIE-2 :D65])
               s (mat/mulv (mat/inverse (mat/cols->mat3x3 r g b)) w)
               f (mat/cols->mat3x3
                  (v/mult r (s 0))
                  (v/mult g (s 1))
                  (v/mult b (s 2)))]
           [f (mat/inverse f)]))
;; => ([0.4124585781796624 0.35757553616579035 0.1804374332363275]
;;     [0.21267395437388842 0.7151510723315807 0.072174973294531]
;;     [0.019333995852171656 0.11919184538859676 0.9503038150446582])


;; spectrum



(def color-matching-functions-data
  (delay (read-edn "color/cmfs_standard_observer.edn")))

(def illuminants-spectrum-data
  (delay (read-edn "color/illuminants.edn")))

(defrecord Spectrum [range lambda value])

(defn spectrum
  "Create Spectrum object from data"
  ([data] (map->Spectrum data))
  ([lambda value] (let [start (reduce min lambda)
                        end (reduce max lambda)]
                    (->Spectrum [start end] lambda value)))
  ([range lambda value] (->Spectrum range lambda value)))

(defn- spectrum-extrapolator
  [interpolator method ^double start ^double end]
  (case method
    :constant (let [sv (double (interpolator start))
                    ev (double (interpolator end))]
                (fn ^double [^double x]
                  (cond
                    (m/< x start) sv
                    (m/> x end) ev
                    :else (interpolator x))))
    :zero (fn ^double [^double x]
            (if (m/between? start end x)
              (interpolator x) 0.0))
    interpolator))

(defn ->spectrum-to-XYZ1
  "Build a converter of spectrum data to XYZ for given observant and illuminant.

  Y is normalized to a 0-1 range.

  Additional parameters:
  * `:interpolation` - method of interpolation, default: `:linear` (also: `:cubic`, `:step`, see `fastmath.interpolation`)
  * `:extrapolation` - what to do outside given range
      - `:trim` - trim ranges to one common range (default)
      - `:constant` - constant value from the boundaries
      - `nil` - extrapolation is done by interpolation function
  * `step` - distance between consecutive frequencies (default: `1.0`)

  Returned function accepts spectrum data which is a map containing:

  * `:lambda` - sequence of frequencies
  * `:values` - sequence of values
  * `:range` - range of frequencies"
  (^Vec4 [] (->spectrum-to-XYZ1 :CIE-2 :D65))
  (^Vec4 [observer illuminant] (->spectrum-to-XYZ1 observer illuminant nil))
  (^Vec4 [observer illuminant {:keys [interpolation ^double step extrapolation]
                               :or {interpolation :linear step 1.0 extrapolation :trim}}]
   (let [trim? (= :trim extrapolation)
         illuminant (get @illuminants-spectrum-data illuminant illuminant)
         observer (get @color-matching-functions-data observer observer)
         [^long si ^long ei] (:range illuminant)
         [^long so ^long eo] (:range observer)
         [^long start ^long end] (if trim?
                                   [(m/max si so) (m/min ei eo)]
                                   [(m/min si so) (m/max ei eo)])
         r (range start (m/+ step end) step)
         lo (:lambda observer)
         i (-> (i/interpolation interpolation (:lambda illuminant) (:value illuminant))
               (spectrum-extrapolator extrapolation si ei))
         x (-> (i/interpolation interpolation lo (:x observer)) (spectrum-extrapolator extrapolation so eo))
         y (-> (i/interpolation interpolation lo (:y observer)) (spectrum-extrapolator extrapolation so eo))
         z (-> (i/interpolation interpolation lo (:z observer)) (spectrum-extrapolator extrapolation so eo))
         n (v/sum (v/emult (map i r) (map y r)))]
     (fn [{:keys [lambda value] :as spectrum}]
       (let [[^long s ^long e] (:range spectrum)
             [^long start1 ^long end1] (if trim?
                                         [(m/max s start) (m/min e end)]
                                         [(m/min s start) (m/max e end)])
             r1 (range start1 (m/+ step end1) step)
             s (-> (i/interpolation interpolation lambda value)
                   (spectrum-extrapolator extrapolation s e))
             sx (map x r1)
             sy (map y r1)
             sz (map z r1)
             sv (map s r1)
             so (map i r1)
             svo (v/emult sv so)]         
         (v/div (Vec3. (v/sum (v/emult svo sx))
                       (v/sum (v/emult svo sy))
                       (v/sum (v/emult svo sz))) n))))))

(defn ->spectrum-to-XYZ
  "Build a converter of spectrum data to XYZ for given observant and illuminant.

  Y is normalized to a 0-100 range.

  Additional parameters:
  * `:interpolation` - method of interpolation, default: `:linear` (also: `:cubic`, `:step`, see `fastmath.interpolation`)
  * `:extrapolation` - what to do outside given range
      - `:trim` - trim ranges to one common range (default)
      - `:constant` - constant value from the boundaries
      - `nil` - extrapolation is done by interpolation function
  * `step` - distance between consecutive frequencies (default: `1.0`)

  Returned function accepts spectrum data which is a map containing:

  * `:lambda` - sequence of frequencies
  * `:values` - sequence of values
  * `:range` - range of frequencies"
  ([] (->spectrum-to-XYZ :CIE-2 :D65))
  ([observer illuminant] (->spectrum-to-XYZ observer illuminant nil))
  ([observer illuminant options] (let [->xyz1 (->spectrum-to-XYZ1 observer illuminant options)]
                                   (fn [spectrum]
                                     (let [^Vec3 xyz1 (->xyz1 spectrum)]
                                       (Vec3. (* 100.0 (.x xyz1))
                                              (* 100.0 (.y xyz1))
                                              (* 100.0 (.z xyz1))))))))




(m/unuse-primitive-operators)
