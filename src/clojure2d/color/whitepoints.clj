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

(defn xy->coordinates
  "Convert xy chroma coordinates to XZxy whitepoint definition."
  (^Vec4 [^Vec2 xy] (xy->coordinates (.x xy) (.y xy)))
  (^Vec4 [^double x ^double y]
   (let [^Vec2 XZ (xy->XZ x y)]
     (Vec4. (.x XZ) (.y XZ) x y))))

(defn XZ->xy
  "Convert XZ coordinates (from XYZ) to xy coordinates (from Yxy). "
  (^Vec2 [^Vec2 XZ] (XZ->xy (.x XZ) (.y XZ)))
  (^Vec2 [^double X ^double Z]
   (-> (Vec2. X 1.0)
       (v/div (+ 1.0 X Z)))))

(defn XZ->coordinates
  "Convert XZ coordinates to XZxy whitepoint definition."
  (^Vec4 [^Vec2 XZ] (XZ->coordinates (.x XZ) (.y XZ)))
  (^Vec4 [^double X ^double Z]
   (let [^Vec2 xy (XZ->xy X Z)]
     (Vec4. X Z (.x xy) (.y xy)))))

(defn- add-Y
  (^Vec3 [XZ] (add-Y XZ 1.0))
  (^Vec3 [^Vec2 XZ Y]
   (Vec3. (.x XZ) Y (.y XZ))))

(def whitepoints (into {}
                     (map (fn [[k v]] [k (v/vec4 v)]))
                     (read-edn "color/whitepoints.edn")))

(defmacro ^:private gen-whitepoints [wp]
  `(do
     ~@(for [k wp
             :let [[X Z x y] (whitepoints k)]]
         `(def ~(symbol (name k)) (Vec4. ~X ~Z ~x ~y)))))

(gen-whitepoints [:CIE-10-FL3-3 :CIE-2-FL3-4 :CIE-2-FL11* :CIE-2015-10-D60* :CIE-10-FL7 :CIE-10-D75 :CIE-2-FL3-9 :CIE-2-HP1 :CIE-2-ID50* :CIE-2015-10-ID50* :CIE-10-LED-BH1* :CIE-2-FL3-13 :CIE-10-FL3-6 :CIE-10-LED-B3 :CIE-2-ISO-PD :CIE-10-FL3-14 :CIE-2015-2-ID65* :CIE-10-HP3* :CIE-2-FL12 :CIE-2-HP4 :CIE-2015-10-FL10* :CIE-2-LED-BH1* :CIE-10-FL3-8 :CIE-2015-2-FL10* :CIE-2-FL2 :CIE-10-A :CIE-10-D60* :CIE-10-FL3-13 :CIE-10-FL3-11* :CIE-10-FL10* :CIE-2-E* :CIE-2-FL8 :CIE-2-ISO-SP :CIE-2015-2-A* :CIE-10-LED-RGB1 :CIE-2015-2-FL4* :CIE-2-FL4 :CIE-10-FL12 :CIE-2015-10-FL2* :CIE-2-FL3-7* :CIE-2-FL3-13* :CIE-2-FL3-4* :CIE-2-FL3-5* :CIE-2015-2-LED-V2* :CIE-10-ISO-SST* :CIE-2015-10-HP4* :CIE-2015-10-FL3-13* :CIE-10-B* :CIE-10-FL3-5* :CIE-2-FL3* :CIE-2015-2-FL3-13* :CIE-2-FL7* :CIE-2-E :CIE-2015-2-E* :CIE-10-FL12* :CIE-2015-10-FL6* :CIE-2015-10-FL3-14* :CIE-2015-10-ISO-ST* :CIE-2-FL3-8* :CIE-10-LED-B1 :CIE-10-FL6* :CIE-10-LED-V2 :CIE-10-C* :CIE-2-FL9* :CIE-2015-2-LED-BH1* :CIE-2-LED-B4* :CIE-10-LED-B2 :CIE-10-D65 :CIE-2015-10-FL11* :CIE-2015-2-LED-V1* :CIE-2015-2-FL3-2* :CIE-10-ISO-SD :CIE-2015-2-FL9* :CIE-2-D50 :CIE-10-FL3-12 :CIE-10-FL3-1* :CIE-2-BLACKMAGIC-WG :CIE-2-FL3-12* :CIE-2-FL3-2 :CIE-2015-10-FL1* :CIE-2015-2-D60* :CIE-2015-2-FL3-10* :CIE-2015-2-ISO-SP* :CIE-2-HP3* :CIE-2015-2-ISO-SST* :CIE-10-HP2* :CIE-10-FL4 :CIE-2-FL3-11 :CIE-10-LED-V1 :CIE-2015-2-HP2* :CIE-10-FL3-15 :CIE-10-LED-B5* :CIE-10-FL11 :CIE-10-ISO-ST* :CIE-10-HP1* :CIE-2-ISO-PF :CIE-2-LED-V2* :CIE-2015-10-LED-V1* :CIE-2015-10-FL4* :CIE-2-ISO-SD :CIE-2-C :CIE-10-FL8* :CIE-2015-10-ISO-SP* :CIE-2-B :CIE-2-ID65 :CIE-2-FL3-3 :CIE-2-ISO-PF* :CIE-10-FL3-2* :CIE-10-FL1 :CIE-10-E :CIE-2015-10-ISO-SPF* :CIE-2015-2-FL1* :CIE-10-FL3-8* :CIE-2015-10-ISO-PF* :CIE-2-D75 :CIE-2-D65* :CIE-2015-10-FL3-12* :CIE-2-FL3-5 :CIE-2-FL3-15* :CIE-10-ID50* :CIE-2-C* :CIE-2015-2-D75* :CIE-2015-10-D75* :CIE-10-LED-BH1 :CIE-2-HP3 :CIE-10-ISO-SP* :CIE-2-FL1 :CIE-2015-10-LED-V2* :CIE-2-ISO-PD* :CIE-10-FL3-7 :CIE-2015-10-HP3* :CIE-10-LED-B2* :CIE-2015-2-LED-B3* :CIE-2-FL11 :CIE-2015-2-FL5* :CIE-2015-10-ISO-SD* :CIE-2015-2-LED-B5* :CIE-10-ISO-PD* :CIE-10-FL3-7* :CIE-2015-10-FL3-7* :CIE-10-LED-B4 :CIE-10-LED-B5 :CIE-2015-2-ISO-PD* :CIE-2-FL3-14 :CIE-10-LED-RGB1* :CIE-2-ISO-ST :CIE-2015-10-B* :CIE-2-HP2* :CIE-10-HP2 :CIE-2015-10-A* :CIE-10-FL3-9* :CIE-2-D50* :CIE-10-FL11* :CIE-10-ISO-PF :CIE-2-ID50 :CIE-10-HP4* :CIE-2-FL3-6* :CIE-2-LED-V1* :CIE-2-LED-B4 :CIE-2-ISO-SST* :CIE-2-FL3-14* :CIE-2015-2-HP1* :CIE-2015-2-LED-B4* :CIE-2-A :CIE-10-E* :CIE-2-FL8* :CIE-2-HP5 :CIE-10-D65* :CIE-2-FL5* :CIE-2015-10-FL3-3* :CIE-2-FL3-11* :CIE-2-ACES :CIE-2015-10-FL12* :CIE-2015-10-HP5* :CIE-2015-10-FL8* :CIE-2-FL2* :CIE-2015-10-FL3-11* :CIE-2015-2-D55* :CIE-2-D65 :CIE-2-FL3 :CIE-2015-2-ISO-PF* :CIE-10-D60 :CIE-2015-2-FL3-15* :CIE-2015-2-FL3-7* :CIE-2-FL3-10* :CIE-2-FL3-7 :CIE-2015-2-LED-B1* :CIE-2-LED-RGB1 :CIE-2015-10-LED-BH1* :CIE-10-FL3-14* :CIE-2-D60 :CIE-2015-10-FL3-2* :CIE-2-FL12* :CIE-2015-2-D50* :CIE-10-ID65 :CIE-2015-2-FL3-3* :CIE-10-D55 :CIE-2015-2-FL2* :CIE-2015-10-FL3* :CIE-10-D55* :CIE-10-FL3* :CIE-2-FL10* :CIE-2-ICC-D50 :CIE-2-A* :CIE-2015-2-FL11* :CIE-2015-2-C* :CIE-2-LED-B5 :CIE-2015-10-D50* :CIE-2015-2-FL3-8* :CIE-10-D75* :CIE-2015-10-FL7* :CIE-2015-10-FL3-10* :CIE-10-A* :CIE-10-FL2* :CIE-2-FL3-2* :CIE-2015-2-FL3-11* :CIE-2-LED-V1 :CIE-2015-10-LED-B5* :CIE-10-ISO-SP :CIE-2-HP5* :CIE-2-LED-BH1 :CIE-2-FL9 :CIE-2015-10-FL9* :CIE-2015-10-ISO-PD* :CIE-10-ISO-PF* :CIE-2-FL1* :CIE-2-LED-B3 :CIE-10-ISO-SPF* :CIE-2-ISO-SD* :CIE-10-ISO-SPF :CIE-2-ISO-SST :CIE-2015-2-FL6* :CIE-2-FL3-9* :CIE-2015-10-FL5* :CIE-2-ISO-SPF* :CIE-10-FL2 :CIE-2-HP2 :CIE-2015-2-FL3* :CIE-10-ID50 :CIE-2-FL7 :CIE-10-LED-B1* :CIE-10-FL4* :CIE-2015-10-C* :CIE-2-FL5 :CIE-10-FL3-4* :CIE-2015-2-FL3-1* :CIE-10-FL3-12* :CIE-2-PLASA :CIE-10-FL3-4 :CIE-10-LED-B3* :CIE-2015-2-LED-B2* :CIE-2015-2-HP4* :CIE-10-LED-V2* :CIE-2015-10-LED-B2* :CIE-2015-2-FL3-5* :CIE-2015-10-FL3-5* :CIE-10-B :CIE-10-D50 :CIE-2-DCI-P3 :CIE-2-FL6* :CIE-10-ISO-SD* :CIE-10-FL3-10 :CIE-2015-2-FL12* :CIE-10-HP3 :CIE-2015-2-ID50* :CIE-2-ID65* :CIE-10-FL3-3* :CIE-10-FL9* :CIE-2015-10-E* :CIE-2015-2-FL3-9* :CIE-10-FL7* :CIE-10-FL5 :CIE-2015-10-FL3-15* :CIE-10-HP5 :CIE-2015-2-FL7* :CIE-2-LED-RGB1* :CIE-2-FL3-8 :CIE-2-ISO-SP* :CIE-10-LED-B4* :CIE-2015-10-FL3-8* :CIE-2-ISO-ST* :CIE-10-FL3-9 :CIE-2-LED-B5* :CIE-2015-10-LED-B3* :CIE-2-LED-B1 :CIE-10-HP5* :CIE-2-FL3-1* :CIE-2015-2-ISO-ST* :CIE-2-FL4* :CIE-2015-10-ID65* :CIE-10-FL3-11 :CIE-10-HP1 :CIE-2-HP1* :CIE-2-FL3-3* :CIE-2015-2-HP5* :CIE-2015-10-LED-RGB1* :CIE-2-D55* :CIE-2-D75* :CIE-2-FL6 :CIE-2015-10-D65* :CIE-2-FL3-10 :CIE-2-FL3-15 :CIE-2-ISO-SPF :CIE-2015-10-FL3-9* :CIE-10-FL3-5 :CIE-10-FL5* :CIE-10-FL3-13* :CIE-2015-10-D55* :CIE-2015-10-FL3-1* :CIE-10-FL6 :CIE-2-HP4* :CIE-10-FL3-10* :CIE-10-FL3-1 :CIE-2015-2-LED-RGB1* :CIE-2015-2-FL3-14* :CIE-2-FL3-12 :CIE-2-LED-B2* :CIE-2015-2-FL3-4* :CIE-2015-10-LED-B4* :CIE-2-FL3-1 :CIE-2-LED-B1* :CIE-2015-2-D65* :CIE-10-HP4 :CIE-10-FL3-2 :CIE-2015-2-FL8* :CIE-10-C :CIE-2-FL10 :CIE-2-FL3-6 :CIE-2015-2-ISO-SD* :CIE-2-LED-B2 :CIE-10-FL8 :CIE-2015-10-FL3-4* :CIE-10-ISO-SST :CIE-10-FL3 :CIE-10-D50* :CIE-10-FL3-15* :CIE-10-ID65* :CIE-2015-2-FL3-6* :CIE-2015-2-HP3* :CIE-2-LED-B3* :CIE-2-LED-V2 :CIE-2015-2-ISO-SPF* :CIE-10-FL1* :CIE-10-ISO-ST :CIE-10-FL9 :CIE-2015-10-FL3-6* :CIE-10-FL10 :CIE-2015-10-LED-B1* :CIE-2-D60* :CIE-10-FL3-6* :CIE-2-D55 :CIE-2015-2-FL3-12* :CIE-10-ISO-PD :CIE-2015-10-HP1* :CIE-2015-2-B* :CIE-2015-10-HP2* :CIE-2-B* :CIE-10-LED-V1* :CIE-2015-10-ISO-SST*])

(defn coordinates
  "Create whitepoint 4 element vector containing X, Z and x,y chroma coordinates"
  (^Vec4 [^double X ^double Z ^double x ^double y] (Vec4. X Z x y))
  (^Vec4 [^double x ^double y] (xy->coordinates x y)))

(defn tristimulus
  "Create XYZ color for given white point as predifined var or x,y coordinates.

  Default is `CIE-2-D65`."
  ([] (tristimulus CIE-2-D65))
  ([^Vec4 coords]
   (Vec3. (.x coords) 1.0 (.y coords)))
  ([^double x ^double y]
   (-> (Vec2. x y) xy->XZ add-Y)))

;; http://www.brucelindbloom.com/index.html?Eqn_ChromAdapt.html

(def chromatic-adaptation-methods
  (let [I         (mat/eye 3)
        bradford  (mat/mat3x3 0.8951 0.2664 -0.1614 -0.7502 1.7135 0.0367 0.0389 -0.0685  1.0296)
        von-kries (mat/mat3x3 0.40024 0.7076 -0.08081 -0.2263 1.16532 0.0457 0.0 0.0 0.9182200)
        sharp (mat/mat3x3 1.2694 -0.0988 -0.1706 -0.8364 1.8006 0.0357 0.0297 -0.0315 1.0018)
        fairchild (mat/mat3x3 0.8562 0.3372 -0.1934 -0.8360 1.8327 0.0033 0.0357 -0.0469 1.0112)
        cat97 (mat/mat3x3 0.8951 -0.7502 0.0389 0.2664 1.7135 0.0685 -0.1614 0.0367 1.0296)
        cat97s (mat/mat3x3 0.8562 0.3372 -0.1934 -0.8360 1.8327 0.0033 0.0357 -0.0469 1.0112)
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
     :cat97s       [cat97s (mat/inverse cat97s)]
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

;; spectrum

(def color-matching-functions-data
  (read-edn "color/cmfs_standard_observer.edn"))

(def illuminants-spectrum-data
  (read-edn "color/illuminants.edn"))

(let [[x y] (get-in color-matching-functions-data [:CIE-2 :range])]
  (def cmf-cie2-x (-> (i/interpolation :sprague
                                     (get-in color-matching-functions-data [:CIE-2 :lambda])
                                     (get-in color-matching-functions-data [:CIE-2 :x]))
                    (i/extrapolation :zero x y)))

  (def cmf-cie2-y (-> (i/interpolation :sprague
                                     (get-in color-matching-functions-data [:CIE-2 :lambda])
                                     (get-in color-matching-functions-data [:CIE-2 :y]))
                    (i/extrapolation :zero x y)))

  (def cmf-cie2-z (-> (i/interpolation :sprague
                                     (get-in color-matching-functions-data [:CIE-2 :lambda])
                                     (get-in color-matching-functions-data [:CIE-2 :z]))
                    (i/extrapolation :zero x y))))

(let [[x y] (get-in color-matching-functions-data [:CIE-10 :range])]
  (def cmf-cie10-x (-> (i/interpolation :sprague
                                      (get-in color-matching-functions-data [:CIE-10 :lambda])
                                      (get-in color-matching-functions-data [:CIE-10 :x]))
                     (i/extrapolation :zero x y)))

  (def cmf-cie10-y (-> (i/interpolation :sprague
                                      (get-in color-matching-functions-data [:CIE-10 :lambda])
                                      (get-in color-matching-functions-data [:CIE-10 :y]))
                     (i/extrapolation :zero x y)))

  (def cmf-cie10-z (-> (i/interpolation :sprague
                                      (get-in color-matching-functions-data [:CIE-10 :lambda])
                                      (get-in color-matching-functions-data [:CIE-10 :z]))
                     (i/extrapolation :zero x y))))

(m/unuse-primitive-operators)
