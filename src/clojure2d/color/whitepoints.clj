(ns clojure2d.color.whitepoints
  "White points, chromatic adaptation and CIE colorimetric data used by [[clojure2d.color]].

  A white point is stored as a `Vec4` `[X Z x y]`: the `X` and `Z` tristimulus values of the white normalised to `Y = 1`, followed by its `x`, `y` chromaticity coordinates. Both pairs describe the same white, so either can be used without recomputation.

  White points are available as the `whitepoints` map (keyword keys) and as vars named `<observer>-<illuminant>`, for example `CIE-2-D65`, `CIE-10-A` or `CIE-2015-2-FL2*`. The observer is `CIE-2` (1931 2°), `CIE-10` (1964 10°), `CIE-2015-2` or `CIE-2015-10` (2015 cone fundamental based). Names without a star are tabulated values; names ending with `*` are computed by integrating the illuminant spectrum with the observer's color matching functions.

  Contents:

  - coordinate helpers: [[xy->XZ]], [[XZ->xy]], [[xy->coordinates]], [[XZ->coordinates]], [[coordinates]], [[tristimulus]],
  - chromatic adaptation: [[chromatic-adaptation-methods]] (13 von Kries type cone response matrices), [[chromatic-adaptation-transform]] and [[chromatic-adaptation-matrix]],
  - spectral data: [[color-matching-functions-data]], [[illuminants-spectrum-data]] and interpolated color matching functions."
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
  "Converts `x`, `y` chromaticity coordinates to `X`, `Z` tristimulus values normalised to `Y = 1`.

  Parameters:

  - `xy` (`Vec2`): chromaticity coordinates `[x y]`.
  - `x`, `y` (doubles): chromaticity coordinates as separate arguments.

  Returns `Vec2` `[X Z]` where `X = x / y` and `Z = (1 - x - y) / y`. For `y = 0` the result is infinite or `NaN`.

  See also [[XZ->xy]], [[xy->coordinates]], [[tristimulus]]."
  (^Vec2 [^Vec2 xy] (xy->XZ (.x xy) (.y xy)))
  (^Vec2 [^double x ^double y]
   (-> (Vec2. x (- 1.0 x y))
       (v/div y))))

(defn xy->coordinates
  "Creates a white point definition `[X Z x y]` from chromaticity coordinates.

  Parameters:

  - `xy` (`Vec2`): chromaticity coordinates `[x y]`.
  - `x`, `y` (doubles): chromaticity coordinates as separate arguments.

  Returns `Vec4` `[X Z x y]` with `X`, `Z` computed by [[xy->XZ]] (normalised to `Y = 1`). For `y = 0` the `X`, `Z` entries are infinite or `NaN`.

  See also [[XZ->coordinates]], [[coordinates]]."
  (^Vec4 [^Vec2 xy] (xy->coordinates (.x xy) (.y xy)))
  (^Vec4 [^double x ^double y]
   (let [^Vec2 XZ (xy->XZ x y)]
     (Vec4. (.x XZ) (.y XZ) x y))))

(defn XZ->xy
  "Converts `X`, `Z` tristimulus values (with `Y = 1`) to `x`, `y` chromaticity coordinates.

  Parameters:

  - `XZ` (`Vec2`): tristimulus values `[X Z]`.
  - `X`, `Z` (doubles): tristimulus values as separate arguments.

  Returns `Vec2` `[x y]` where `x = X / (1 + X + Z)` and `y = 1 / (1 + X + Z)`. For `X + Z = -1` the result is infinite or `NaN`.

  See also [[xy->XZ]], [[XZ->coordinates]]."
  (^Vec2 [^Vec2 XZ] (XZ->xy (.x XZ) (.y XZ)))
  (^Vec2 [^double X ^double Z]
   (-> (Vec2. X 1.0)
       (v/div (+ 1.0 X Z)))))

(defn XZ->coordinates
  "Creates a white point definition `[X Z x y]` from `X`, `Z` tristimulus values (with `Y = 1`).

  Parameters:

  - `XZ` (`Vec2`): tristimulus values `[X Z]`.
  - `X`, `Z` (doubles): tristimulus values as separate arguments.

  Returns `Vec4` `[X Z x y]` with `x`, `y` computed by [[XZ->xy]].

  See also [[xy->coordinates]], [[coordinates]]."
  (^Vec4 [^Vec2 XZ] (XZ->coordinates (.x XZ) (.y XZ)))
  (^Vec4 [^double X ^double Z]
   (let [^Vec2 xy (XZ->xy X Z)]
     (Vec4. X Z (.x xy) (.y xy)))))

(defn- add-Y
  (^Vec3 [XZ] (add-Y XZ 1.0))
  (^Vec3 [^Vec2 XZ ^double Y]
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
  "Creates a white point definition `[X Z x y]`.

  Parameters:

  - `X`, `Z`, `x`, `y` (doubles): all four entries, stored as given without a consistency check.
  - `x`, `y` (doubles): chromaticity coordinates only, `X` and `Z` are computed as in [[xy->coordinates]].

  Returns `Vec4` `[X Z x y]`.

  See also [[XZ->coordinates]], [[tristimulus]]."
  (^Vec4 [^double X ^double Z ^double x ^double y] (Vec4. X Z x y))
  (^Vec4 [^double x ^double y] (xy->coordinates x y)))

(defn tristimulus
  "Returns the XYZ tristimulus values of a white point, normalised to `Y = 1`.

  Parameters:

  - no arguments: the white point `CIE-2-D65`.
  - `coords` (`Vec4`): a white point definition `[X Z x y]`, for example `CIE-2-C`; only `X` and `Z` are read.
  - `x`, `y` (doubles): chromaticity coordinates, converted by [[xy->XZ]].

  Returns `Vec3` `[X 1.0 Z]`. For `y = 0` the `X`, `Z` entries are infinite or `NaN`.

  See also [[coordinates]], [[chromatic-adaptation-matrix]]."
  ([] (tristimulus CIE-2-D65))
  ([^Vec4 coords]
   (Vec3. (.x coords) 1.0 (.y coords)))
  ([^double x ^double y]
   (-> (Vec2. x y) xy->XZ add-Y)))

;; http://www.brucelindbloom.com/index.html?Eqn_ChromAdapt.html

(def ^{:doc "Cone response matrices of the von Kries type chromatic adaptation transforms, keyed by method.

  Each value is a pair `[Ma Ma-1]` of `Mat3x3`: `Ma` converts XYZ to cone response (LMS like) space, `Ma-1` is its inverse. Methods:

  - `:xyz-scaling` - identity, scaling in XYZ,
  - `:bradford` - Bradford (Lam 1985),
  - `:von-kries` - von Kries, Hunt-Pointer-Estevez normalised to D65,
  - `:sharp` - Sharp (Süsstrunk, Holm and Finlayson 2001),
  - `:fairchild` - Fairchild 2001 revision of CMCCAT97,
  - `:cat97` - CMCCAT97,
  - `:cat97s` - the same matrix as `:fairchild`,
  - `:cat2000` - CMCCAT2000,
  - `:cat02` - CIECAM02,
  - `:cat02brill2008` - CAT02 with the Brill and Süsstrunk 2008 correction,
  - `:cat16` - CAM16,
  - `:bianco2010`, `:bianco2010-pc` - Bianco and Schettini 2010, plain and with a positivity constraint.

  See also [[chromatic-adaptation-transform]], [[chromatic-adaptation-matrix]]."}
  chromatic-adaptation-methods
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

(defn chromatic-adaptation-transform
  "Returns the cone response matrices of a chromatic adaptation method.

  Parameters:

  - `adaptation-method` (keyword): a key of [[chromatic-adaptation-methods]], for example `:bradford` or `:cat16`.

  Returns a pair `[Ma Ma-1]` of `Mat3x3`: XYZ to cone response and its inverse.

  Throws `ex-info` with `{:method adaptation-method}` for an unknown method.

  See also [[chromatic-adaptation-matrix]]."
  [adaptation-method]
  (or (chromatic-adaptation-methods adaptation-method)
      (throw (ex-info (str "Unknown chromatic adaptation method: " adaptation-method)
                      {:method adaptation-method}))))

(defn chromatic-adaptation-matrix
  "Calculates the von Kries type chromatic adaptation matrix between two white points.

  The matrix is `Ma-1 · diag(Ma · wd / Ma · ws) · Ma`, where `ws` and `wd` are the white point tristimulus values (with `Y = 1`) and `Ma` the cone response matrix of the method.

  Parameters:

  - `adaptation-method` (keyword, required): a key of [[chromatic-adaptation-methods]]: `:xyz-scaling`, `:bradford`, `:von-kries`, `:sharp`, `:fairchild`, `:cat97`, `:cat97s`, `:cat2000`, `:cat02`, `:cat02brill2008`, `:cat16`, `:bianco2010`, `:bianco2010-pc`.
  - `source-wp`, `destination-wp` (`Vec4`): white points `[X Z x y]`, for example `CIE-2-C` or `CIE-2-D65`.

  Returns a `Mat3x3` converting XYZ under the source white point to XYZ under the destination white point; equal white points give the identity.

  Throws `ex-info` with `{:method adaptation-method}` for an unknown method.

  See also [[chromatic-adaptation-transform]], [[tristimulus]]."
  (^Mat3x3 [adaptation-method source-wp destination-wp]
   (let [ws (tristimulus source-wp)
         wd (tristimulus destination-wp)
         [Ma Ma-1] (chromatic-adaptation-transform adaptation-method)
         s (mat/mulv Ma ws)
         d (mat/mulv Ma wd)
         ^Vec3 r (v/ediv d s)
         diag (mat/diagonal (.x r) (.y r) (.z r))
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
