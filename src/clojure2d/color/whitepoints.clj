(ns clojure2d.color.whitepoints
  "White points, chromatic adaptation and CIE colorimetric data used by [[clojure2d.color]].

  A white point is stored as a `Vec4` `[X Z x y]`: the `X` and `Z` tristimulus values of the white normalised to `Y = 1`, followed by its `x`, `y` chromaticity coordinates. Both pairs describe the same white, so either can be used without recomputation.

  White points are available as the `whitepoints` map (keyword keys) and as vars named `<observer>-<illuminant>`, for example `CIE-2-D65`, `CIE-10-A` or `CIE-2015-2-FL2*`. The observer is `CIE-2` (1931 2°), `CIE-10` (1964 10°), `CIE-2015-2` or `CIE-2015-10` (2015 cone fundamental based). Names ending with `*` are computed by integrating the illuminant spectrum with the observer's color matching functions; names without a star are tabulated chromaticities, except the D-series (D50, D55, D60, D65, D75) of the 1931 and 1964 observers, which are 1 nm integrations of CIE daylight spectra (see [[whitepoints]]).

  Contents:

  - coordinate helpers: [[xy->XZ]], [[XZ->xy]], [[xy->coordinates]], [[XZ->coordinates]], [[coordinates]], [[tristimulus]],
  - chromatic adaptation: [[chromatic-adaptation-methods]] (14 von Kries type cone response matrices), [[chromatic-adaptation-transform]] and [[chromatic-adaptation-matrix]],
  - spectral data: [[color-matching-functions-data]], [[illuminants-spectrum-data]], color matching functions [[cmf]] (and `cmf-cie2-*`, `cmf-cie10-*`),
  - spectrum to XYZ: [[spectrum]], [[->spectrum-to-XYZ1]],
  - white point lookup: [[whitepoint]],
  - correlated color temperature: [[CCT->xy]], [[xy->CCT]], [[CCT->uv]], [[uv->CCT]], [[CCT->mired]], [[mired->CCT]], [[daylight-locus]],
  - light source spectra: [[planck-law]], [[blackbody-spectrum]], [[illuminant-A-spectrum]], [[illuminant-D-spectrum]]."
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [fastmath.matrix :as mat]
            [fastmath.interpolation :as i]
            [fastmath.optimization :as opt]
            [clojure.java.io :refer [resource input-stream]]
            [clojure.edn :as edn])
  (:import [fastmath.vector Vec2 Vec3 Vec4]
           [fastmath.matrix Mat3x3]
           [fastmath.java Array]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

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
   (-> (Vec2. x (m/- 1.0 x y))
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
       (v/div (m/+ 1.0 X Z)))))

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

(def ^{:doc "All white points as a map from keyword to `Vec4` `[X Z x y]` (`Y = 1`).

  Keys are `:<observer>-<illuminant>` with an optional `*` suffix, for example `:CIE-2-D65` or `:CIE-2015-10-FL2*`; each key is also available as a var of the same name, for example `CIE-2-D65`. Values without a star are tabulated chromaticities, except the D-series entries (D50, D55, D60, D65, D75) which are 1 nm integrations of CIE daylight spectra (`CIE-2-D65` rounded to 6 decimals, see each var for the spectrum); values with a star are computed from the illuminant spectrum and the observer's color matching functions, as [[->spectrum-to-XYZ1]] does with default options.

  Data generated with colour-science (`utils/whitepoints/gen_resources.py`).

  See also [[coordinates]], [[tristimulus]]."}
  whitepoints
  (into {}
        (map (fn [[k v]] [k (v/vec4 v)]))
        (read-edn "color/whitepoints.edn")))

(def ^:private observer-names {"2" "CIE 1931 2°"
                               "10" "CIE 1964 10°"
                               "2015-2" "CIE 2015 2°"
                               "2015-10" "CIE 2015 10°"})

(def ^:private illuminant-names
  {"A" "CIE standard illuminant A (incandescent, 2856 K)"
   "B" "CIE illuminant B (direct sunlight, obsolete)"
   "C" "CIE illuminant C (average daylight, obsolete)"
   "D50" "CIE illuminant D50 (horizon daylight, 5003 K)"
   "D55" "CIE illuminant D55 (mid-morning daylight, 5503 K)"
   "D60" "CIE illuminant D60 (daylight, 6004 K)"
   "D65" "CIE standard illuminant D65 (noon daylight, 6504 K)"
   "D75" "CIE illuminant D75 (north sky daylight, 7504 K)"
   "E" "the equal-energy illuminant E"
   "ID50" "CIE indoor daylight illuminant ID50"
   "ID65" "CIE indoor daylight illuminant ID65"
   "ISO-PD" "ISO 7589 photographic daylight"
   "ISO-PF" "ISO 7589 photoflood"
   "ISO-SD" "ISO 7589 sensitometric daylight"
   "ISO-SP" "ISO 7589 sensitometric printer"
   "ISO-SPF" "ISO 7589 sensitometric photoflood"
   "ISO-SST" "ISO 7589 sensitometric studio tungsten"
   "ISO-ST" "ISO 7589 studio tungsten"
   "ACES" "the ACES white point (about D60)"
   "BLACKMAGIC-WG" "the Blackmagic Wide Gamut white point"
   "DCI-P3" "the DCI-P3 projector white point"
   "ICC-D50" "the ICC profile connection space D50 white"
   "PLASA" "the PLASA ANSI E1.54 white point"})

(defn- illuminant-name
  [^String illuminant]
  (or (illuminant-names illuminant)
      (when-let [[_ n] (re-matches #"FL3-(\d+)" illuminant)] (str "CIE fluorescent illuminant FL3." n))
      (when (re-matches #"FL\d+" illuminant) (str "CIE fluorescent illuminant " illuminant))
      (when (re-matches #"HP\d" illuminant) (str "CIE high pressure discharge lamp illuminant " illuminant))
      (when (re-matches #"LED-.+" illuminant) (str "CIE LED illuminant " illuminant))
      illuminant))

(defn- whitepoint-doc
  [k]
  (let [[_ observer illuminant computed] (re-matches #"CIE-(2015-2|2015-10|2|10)-(.+?)(\*)?" (name k))
        provenance (cond
                     computed "Computed: 1 nm sum of the illuminant spectrum and the observer's color matching functions over the observer's range (as [[->spectrum-to-XYZ1]] with default options)."
                     (= k :CIE-2-D65) "Tabulated: 1 nm integration of the CIE D65 spectrum rounded to 6 decimals, (0.312727, 0.329023)."
                     (#{"D50" "D65"} illuminant) "1 nm integration of the official CIE spectrum (300-830 nm, linear interpolation), full precision."
                     (#{"D55" "D75"} illuminant) "1 nm integration of the CIE 15 table (to 780 nm, colour-science) completed to 830 nm with the CIE daylight basis (linear interpolation), full precision."
                     (= "D60" illuminant) "1 nm integration of the D-series spectrum computed from the CIE daylight basis (colour-science, 300-830 nm, linear interpolation), full precision."
                     :else "Tabulated chromaticity (colour-science).")]
    (str "White point `[X Z x y]` (`Y = 1`) of " (illuminant-name illuminant)
         " for the " (observer-names observer) " observer.\n\n  " provenance
         "\n\n  See also [[whitepoints]], [[tristimulus]].")))

(defmacro ^:private gen-whitepoints [wp]
  `(do
     ~@(for [k wp
             :let [[X Z x y] (whitepoints k)]]
         `(def ~(with-meta (symbol (name k)) {:doc (whitepoint-doc k)}) (Vec4. ~X ~Z ~x ~y)))))

(gen-whitepoints [:CIE-10-FL3-3 :CIE-2-FL3-4 :CIE-2-FL11* :CIE-2015-10-D60* :CIE-10-FL7 :CIE-10-D75 :CIE-2-FL3-9 :CIE-2-HP1 :CIE-2-ID50* :CIE-2015-10-ID50* :CIE-10-LED-BH1* :CIE-2-FL3-13 :CIE-10-FL3-6 :CIE-10-LED-B3 :CIE-2-ISO-PD :CIE-10-FL3-14 :CIE-2015-2-ID65* :CIE-10-HP3* :CIE-2-FL12 :CIE-2-HP4 :CIE-2015-10-FL10* :CIE-2-LED-BH1* :CIE-10-FL3-8 :CIE-2015-2-FL10* :CIE-2-FL2 :CIE-10-A :CIE-10-D60* :CIE-10-FL3-13 :CIE-10-FL3-11* :CIE-10-FL10* :CIE-2-E* :CIE-2-FL8 :CIE-2-ISO-SP :CIE-2015-2-A* :CIE-10-LED-RGB1 :CIE-2015-2-FL4* :CIE-2-FL4 :CIE-10-FL12 :CIE-2015-10-FL2* :CIE-2-FL3-7* :CIE-2-FL3-13* :CIE-2-FL3-4* :CIE-2-FL3-5* :CIE-2015-2-LED-V2* :CIE-10-ISO-SST* :CIE-2015-10-HP4* :CIE-2015-10-FL3-13* :CIE-10-B* :CIE-10-FL3-5* :CIE-2-FL3* :CIE-2015-2-FL3-13* :CIE-2-FL7* :CIE-2-E :CIE-2015-2-E* :CIE-10-FL12* :CIE-2015-10-FL6* :CIE-2015-10-FL3-14* :CIE-2015-10-ISO-ST* :CIE-2-FL3-8* :CIE-10-LED-B1 :CIE-10-FL6* :CIE-10-LED-V2 :CIE-10-C* :CIE-2-FL9* :CIE-2015-2-LED-BH1* :CIE-2-LED-B4* :CIE-10-LED-B2 :CIE-10-D65 :CIE-2015-10-FL11* :CIE-2015-2-LED-V1* :CIE-2015-2-FL3-2* :CIE-10-ISO-SD :CIE-2015-2-FL9* :CIE-2-D50 :CIE-10-FL3-12 :CIE-10-FL3-1* :CIE-2-BLACKMAGIC-WG :CIE-2-FL3-12* :CIE-2-FL3-2 :CIE-2015-10-FL1* :CIE-2015-2-D60* :CIE-2015-2-FL3-10* :CIE-2015-2-ISO-SP* :CIE-2-HP3* :CIE-2015-2-ISO-SST* :CIE-10-HP2* :CIE-10-FL4 :CIE-2-FL3-11 :CIE-10-LED-V1 :CIE-2015-2-HP2* :CIE-10-FL3-15 :CIE-10-LED-B5* :CIE-10-FL11 :CIE-10-ISO-ST* :CIE-10-HP1* :CIE-2-ISO-PF :CIE-2-LED-V2* :CIE-2015-10-LED-V1* :CIE-2015-10-FL4* :CIE-2-ISO-SD :CIE-2-C :CIE-10-FL8* :CIE-2015-10-ISO-SP* :CIE-2-B :CIE-2-ID65 :CIE-2-FL3-3 :CIE-2-ISO-PF* :CIE-10-FL3-2* :CIE-10-FL1 :CIE-10-E :CIE-2015-10-ISO-SPF* :CIE-2015-2-FL1* :CIE-10-FL3-8* :CIE-2015-10-ISO-PF* :CIE-2-D75 :CIE-2-D65* :CIE-2015-10-FL3-12* :CIE-2-FL3-5 :CIE-2-FL3-15* :CIE-10-ID50* :CIE-2-C* :CIE-2015-2-D75* :CIE-2015-10-D75* :CIE-10-LED-BH1 :CIE-2-HP3 :CIE-10-ISO-SP* :CIE-2-FL1 :CIE-2015-10-LED-V2* :CIE-2-ISO-PD* :CIE-10-FL3-7 :CIE-2015-10-HP3* :CIE-10-LED-B2* :CIE-2015-2-LED-B3* :CIE-2-FL11 :CIE-2015-2-FL5* :CIE-2015-10-ISO-SD* :CIE-2015-2-LED-B5* :CIE-10-ISO-PD* :CIE-10-FL3-7* :CIE-2015-10-FL3-7* :CIE-10-LED-B4 :CIE-10-LED-B5 :CIE-2015-2-ISO-PD* :CIE-2-FL3-14 :CIE-10-LED-RGB1* :CIE-2-ISO-ST :CIE-2015-10-B* :CIE-2-HP2* :CIE-10-HP2 :CIE-2015-10-A* :CIE-10-FL3-9* :CIE-2-D50* :CIE-10-FL11* :CIE-10-ISO-PF :CIE-2-ID50 :CIE-10-HP4* :CIE-2-FL3-6* :CIE-2-LED-V1* :CIE-2-LED-B4 :CIE-2-ISO-SST* :CIE-2-FL3-14* :CIE-2015-2-HP1* :CIE-2015-2-LED-B4* :CIE-2-A :CIE-10-E* :CIE-2-FL8* :CIE-2-HP5 :CIE-10-D65* :CIE-2-FL5* :CIE-2015-10-FL3-3* :CIE-2-FL3-11* :CIE-2-ACES :CIE-2015-10-FL12* :CIE-2015-10-HP5* :CIE-2015-10-FL8* :CIE-2-FL2* :CIE-2015-10-FL3-11* :CIE-2015-2-D55* :CIE-2-D65 :CIE-2-FL3 :CIE-2015-2-ISO-PF* :CIE-10-D60 :CIE-2015-2-FL3-15* :CIE-2015-2-FL3-7* :CIE-2-FL3-10* :CIE-2-FL3-7 :CIE-2015-2-LED-B1* :CIE-2-LED-RGB1 :CIE-2015-10-LED-BH1* :CIE-10-FL3-14* :CIE-2-D60 :CIE-2015-10-FL3-2* :CIE-2-FL12* :CIE-2015-2-D50* :CIE-10-ID65 :CIE-2015-2-FL3-3* :CIE-10-D55 :CIE-2015-2-FL2* :CIE-2015-10-FL3* :CIE-10-D55* :CIE-10-FL3* :CIE-2-FL10* :CIE-2-ICC-D50 :CIE-2-A* :CIE-2015-2-FL11* :CIE-2015-2-C* :CIE-2-LED-B5 :CIE-2015-10-D50* :CIE-2015-2-FL3-8* :CIE-10-D75* :CIE-2015-10-FL7* :CIE-2015-10-FL3-10* :CIE-10-A* :CIE-10-FL2* :CIE-2-FL3-2* :CIE-2015-2-FL3-11* :CIE-2-LED-V1 :CIE-2015-10-LED-B5* :CIE-10-ISO-SP :CIE-2-HP5* :CIE-2-LED-BH1 :CIE-2-FL9 :CIE-2015-10-FL9* :CIE-2015-10-ISO-PD* :CIE-10-ISO-PF* :CIE-2-FL1* :CIE-2-LED-B3 :CIE-10-ISO-SPF* :CIE-2-ISO-SD* :CIE-10-ISO-SPF :CIE-2-ISO-SST :CIE-2015-2-FL6* :CIE-2-FL3-9* :CIE-2015-10-FL5* :CIE-2-ISO-SPF* :CIE-10-FL2 :CIE-2-HP2 :CIE-2015-2-FL3* :CIE-10-ID50 :CIE-2-FL7 :CIE-10-LED-B1* :CIE-10-FL4* :CIE-2015-10-C* :CIE-2-FL5 :CIE-10-FL3-4* :CIE-2015-2-FL3-1* :CIE-10-FL3-12* :CIE-2-PLASA :CIE-10-FL3-4 :CIE-10-LED-B3* :CIE-2015-2-LED-B2* :CIE-2015-2-HP4* :CIE-10-LED-V2* :CIE-2015-10-LED-B2* :CIE-2015-2-FL3-5* :CIE-2015-10-FL3-5* :CIE-10-B :CIE-10-D50 :CIE-2-DCI-P3 :CIE-2-FL6* :CIE-10-ISO-SD* :CIE-10-FL3-10 :CIE-2015-2-FL12* :CIE-10-HP3 :CIE-2015-2-ID50* :CIE-2-ID65* :CIE-10-FL3-3* :CIE-10-FL9* :CIE-2015-10-E* :CIE-2015-2-FL3-9* :CIE-10-FL7* :CIE-10-FL5 :CIE-2015-10-FL3-15* :CIE-10-HP5 :CIE-2015-2-FL7* :CIE-2-LED-RGB1* :CIE-2-FL3-8 :CIE-2-ISO-SP* :CIE-10-LED-B4* :CIE-2015-10-FL3-8* :CIE-2-ISO-ST* :CIE-10-FL3-9 :CIE-2-LED-B5* :CIE-2015-10-LED-B3* :CIE-2-LED-B1 :CIE-10-HP5* :CIE-2-FL3-1* :CIE-2015-2-ISO-ST* :CIE-2-FL4* :CIE-2015-10-ID65* :CIE-10-FL3-11 :CIE-10-HP1 :CIE-2-HP1* :CIE-2-FL3-3* :CIE-2015-2-HP5* :CIE-2015-10-LED-RGB1* :CIE-2-D55* :CIE-2-D75* :CIE-2-FL6 :CIE-2015-10-D65* :CIE-2-FL3-10 :CIE-2-FL3-15 :CIE-2-ISO-SPF :CIE-2015-10-FL3-9* :CIE-10-FL3-5 :CIE-10-FL5* :CIE-10-FL3-13* :CIE-2015-10-D55* :CIE-2015-10-FL3-1* :CIE-10-FL6 :CIE-2-HP4* :CIE-10-FL3-10* :CIE-10-FL3-1 :CIE-2015-2-LED-RGB1* :CIE-2015-2-FL3-14* :CIE-2-FL3-12 :CIE-2-LED-B2* :CIE-2015-2-FL3-4* :CIE-2015-10-LED-B4* :CIE-2-FL3-1 :CIE-2-LED-B1* :CIE-2015-2-D65* :CIE-10-HP4 :CIE-10-FL3-2 :CIE-2015-2-FL8* :CIE-10-C :CIE-2-FL10 :CIE-2-FL3-6 :CIE-2015-2-ISO-SD* :CIE-2-LED-B2 :CIE-10-FL8 :CIE-2015-10-FL3-4* :CIE-10-ISO-SST :CIE-10-FL3 :CIE-10-D50* :CIE-10-FL3-15* :CIE-10-ID65* :CIE-2015-2-FL3-6* :CIE-2015-2-HP3* :CIE-2-LED-B3* :CIE-2-LED-V2 :CIE-2015-2-ISO-SPF* :CIE-10-FL1* :CIE-10-ISO-ST :CIE-10-FL9 :CIE-2015-10-FL3-6* :CIE-10-FL10 :CIE-2015-10-LED-B1* :CIE-2-D60* :CIE-10-FL3-6* :CIE-2-D55 :CIE-2015-2-FL3-12* :CIE-10-ISO-PD :CIE-2015-10-HP1* :CIE-2015-2-B* :CIE-2015-10-HP2* :CIE-2-B* :CIE-10-LED-V1* :CIE-2015-10-ISO-SST*])

(defn whitepoint
  "Returns the white point of an illuminant for a standard observer.

  Parameters:

  - `observer` (keyword): `:CIE-2`, `:CIE-10`, `:CIE-2015-2` or `:CIE-2015-10`.
  - `illuminant` (keyword): illuminant name as in the white point vars, for example `:D65`, `:A` or `:FL3-1`; add `*` (`:D65*`) for the value computed from the spectrum. The 2015 observers have computed values only.

  Returns `Vec4` `[X Z x y]`, the same value as the var `<observer>-<illuminant>` (for example `CIE-2-D65`).

  Throws `ex-info` with `{:observer observer :illuminant illuminant}` when no such white point exists.

  See also [[whitepoints]], [[tristimulus]]."
  [observer illuminant]
  (or (whitepoints (keyword (str (name observer) "-" (name illuminant))))
      (throw (ex-info (str "No white point for observer " observer " and illuminant " illuminant)
                      {:observer observer :illuminant illuminant}))))

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
  - `:cat97` - CMCCAT97 (CIECAM97s), the same matrix as `:bradford`; the nonlinear blue channel exponent of the full model is not applied,
  - `:cat97-colour` - the CMCCAT97 matrix of colour-science (`CAT_CMCCAT97`, after Westland et al. 2012): the Bradford matrix transposed with one sign changed, a misprint kept for compatibility with colour-science,
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
        cat97-colour (mat/mat3x3 0.8951 -0.7502 0.0389 0.2664 1.7135 0.0685 -0.1614 0.0367 1.0296)
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
     :cat97       [bradford (mat/inverse bradford)]
     :cat97-colour [cat97-colour (mat/inverse cat97-colour)]
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

  - `adaptation-method` (keyword, required): a key of [[chromatic-adaptation-methods]]: `:xyz-scaling`, `:bradford`, `:von-kries`, `:sharp`, `:fairchild`, `:cat97`, `:cat97-colour`, `:cat97s`, `:cat2000`, `:cat02`, `:cat02brill2008`, `:cat16`, `:bianco2010`, `:bianco2010-pc`.
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

(def ^{:doc "CIE standard observer color matching functions, keyed by observer.

  Keys: `:CIE-2` (CIE 1931 2°), `:CIE-10` (CIE 1964 10°), `:CIE-2015-2`, `:CIE-2015-10` (CIE 2015 cone fundamental based). Each value is a map with `:lambda` (wavelengths in nm, 1 nm step), `:x`, `:y`, `:z` (the color matching functions) and `:range` (`[first last]` wavelength: 360-830 nm for 1931/1964, 390-830 nm for 2015).

  Data from colour-science.

  See also [[cmf]], [[illuminants-spectrum-data]]."}
  color-matching-functions-data
  (read-edn "color/cmfs_standard_observer.edn"))

(def ^{:doc "Relative spectral power distributions of CIE and other standard illuminants, keyed by illuminant.

  Keys are the illuminant parts of white point names: `:A`, `:B`, `:C`, `:D50`, `:D55`, `:D60`, `:D65`, `:D75`, `:E`, `:FL1` - `:FL12`, `:FL3-1` - `:FL3-15`, `:HP1` - `:HP5`, `:ID50`, `:ID65`, `:LED-B1` - `:LED-B5`, `:LED-BH1`, `:LED-RGB1`, `:LED-V1`, `:LED-V2` and ISO 7589 illuminants `:ISO-PD`, `:ISO-PF`, `:ISO-SD`, `:ISO-SP`, `:ISO-SPF`, `:ISO-SST`, `:ISO-ST`. Each value is a spectrum map (see [[spectrum]]): `:lambda` (nm), `:value`, `:range`.

  Data from colour-science.

  See also [[color-matching-functions-data]], [[->spectrum-to-XYZ1]]."}
  illuminants-spectrum-data
  (read-edn "color/illuminants.edn"))

(defn- cmf-interpolators
  [observer]
  (let [{:keys [lambda x y z]} (color-matching-functions-data observer)
        [lo hi] (:range (color-matching-functions-data observer))
        interpolator (fn [values] (-> (i/interpolation :sprague lambda values)
                                     (i/extrapolation :zero lo hi)))]
    [(interpolator x) (interpolator y) (interpolator z)]))

(def ^:private cmfs-delays
  (into {} (map (fn [k] [k (delay (cmf-interpolators k))])) (keys color-matching-functions-data)))

(defn cmf
  "Returns the color matching functions of a standard observer as interpolated functions of wavelength.

  Parameters:

  - `observer` (keyword): `:CIE-2`, `:CIE-10`, `:CIE-2015-2` or `:CIE-2015-10`.

  Returns a vector `[x y z]` of functions `(fn [lambda])`, `lambda` in nm, returning the color matching function value (`y` peaks at 1.0). Values between the 1 nm data samples are interpolated with the Sprague method, values outside the observer's range (see [[color-matching-functions-data]]) are 0. The functions are built once per observer, on first use.

  Throws `ex-info` with `{:observer observer}` for an unknown observer.

  See also [[color-matching-functions-data]], [[->spectrum-to-XYZ1]]."
  [observer]
  (if-let [d (cmfs-delays observer)]
    @d
    (throw (ex-info (str "Unknown observer: " observer) {:observer observer}))))

(def ^{:doc "CIE 1931 2° color matching function x̄ as a function of wavelength in nm, see [[cmf]]."}
  cmf-cie2-x ((cmf :CIE-2) 0))
(def ^{:doc "CIE 1931 2° color matching function ȳ as a function of wavelength in nm, see [[cmf]]."}
  cmf-cie2-y ((cmf :CIE-2) 1))
(def ^{:doc "CIE 1931 2° color matching function z̄ as a function of wavelength in nm, see [[cmf]]."}
  cmf-cie2-z ((cmf :CIE-2) 2))
(def ^{:doc "CIE 1964 10° color matching function x̄ as a function of wavelength in nm, see [[cmf]]."}
  cmf-cie10-x ((cmf :CIE-10) 0))
(def ^{:doc "CIE 1964 10° color matching function ȳ as a function of wavelength in nm, see [[cmf]]."}
  cmf-cie10-y ((cmf :CIE-10) 1))
(def ^{:doc "CIE 1964 10° color matching function z̄ as a function of wavelength in nm, see [[cmf]]."}
  cmf-cie10-z ((cmf :CIE-10) 2))

(defn spectrum
  "Creates a spectrum map from sampled spectral data.

  Parameters:

  - `lambda` (sequence of numbers): wavelengths in nm.
  - `value` (sequence of numbers): spectral values at `lambda`, same length.
  - `wavelength-range` (`[start end]`, optional): domain of the spectrum; default is the minimum and maximum of `lambda`.

  Returns a map `{:range wavelength-range :lambda lambda :value value}`, the format of [[illuminants-spectrum-data]] entries and of the spectra accepted by [[->spectrum-to-XYZ1]].

  See also [[->spectrum-to-XYZ1]]."
  ([lambda value] (spectrum [(reduce m/min lambda) (reduce m/max lambda)] lambda value))
  ([wavelength-range lambda value] {:range wavelength-range :lambda lambda :value value}))

(def ^:private spectrum-extrapolations #{:constant :zero})

(defn- uniform-wavelengths?
  [lambda]
  (let [diffs (map (fn [^double a ^double b] (m/- b a)) lambda (rest lambda))
        d (double (first diffs))]
    (every? (fn [^double x] (m/<= (m/abs (m/- x d)) (m/* m/EPSILON d))) diffs)))

(defn- auto-interpolation
  "colour-science rule: Sprague for uniform data, cubic spline (not-a-knot) otherwise."
  [lambda]
  (let [n (count lambda)]
    (cond
      (and (m/>= n 6) (uniform-wavelengths? lambda)) :sprague
      (m/>= n 4) :b-spline
      :else :linear)))

(defn- spectrum-interpolator
  [interpolation extrapolation {:keys [lambda value] :as spectrum-data}]
  (let [[lo hi] (or (:range spectrum-data) [(reduce min lambda) (reduce max lambda)])
        interpolation (if (= :auto interpolation) (auto-interpolation lambda) interpolation)]
    (-> (i/interpolation interpolation lambda value)
        (i/extrapolation extrapolation lo hi))))

(defn ->spectrum-to-XYZ1
  "Builds a converter of a spectrum (reflectance or transmittance) to XYZ under an illuminant and a standard observer.

  The XYZ values are sums over the observer's wavelength range (360-830 nm or 390-830 nm), sampled every `step` nm: `X = Σ S(λ) R(λ) x̄(λ) / Σ S(λ) ȳ(λ)` (similarly `Y`, `Z`), where `S` is the illuminant, `R` the spectrum and `x̄`, `ȳ`, `z̄` the color matching functions. The illuminant, the spectrum and the color matching functions are interpolated at the sampled wavelengths; the illuminant and the spectrum are extended beyond their own ranges according to `:extrapolation`. A perfect reflector (`R = 1`) gives `Y = 1` and the illuminant's white point.

  Parameters:

  - `observer` (optional, default `:CIE-2`): a key of [[color-matching-functions-data]] or a map of the same form.
  - `illuminant` (optional, default `:D65`): a key of [[illuminants-spectrum-data]] or a spectrum map (see [[spectrum]]).
  - `options` (map, optional):
      - `:interpolation` - interpolation method of the spectrum, default `:auto`: Sprague for uniformly spaced wavelengths and at least 6 samples, cubic spline (`:b-spline`, not-a-knot) for at least 4 samples, linear otherwise (the colour-science rule). Any 1D method of `fastmath.interpolation` can be given instead.
      - `:illuminant-interpolation` - interpolation method of the illuminant, default `:linear` (safe for line spectra of fluorescent and LED illuminants, as colour-science); `:auto` or any 1D method of `fastmath.interpolation`.
      - `:extrapolation` - values of the illuminant and the spectrum outside their ranges: `:constant` (default, the boundary values, as CIE 15 recommends) or `:zero`.
      - `:step` - positive sampling interval in nm, default `1.0`.

  The color matching functions are interpolated with the `:auto` rule (Sprague for the 1 nm standard observers, exact at integer wavelengths) and are 0 outside their range.

  Returns a function of a spectrum map (`:lambda` in ascending order, `:value` and optional `:range`, see [[spectrum]]) returning `Vec3` `[X Y Z]` with `Y` in 0-1.

  Throws `ex-info` with `{:extrapolation e}` for an unsupported extrapolation, `{:observer o}` or `{:illuminant i}` for an unknown key.

  See also `clojure2d.color/->spectrum-to-XYZ` (Y in 0-100), [[cmf]]."
  ([] (->spectrum-to-XYZ1 :CIE-2 :D65))
  ([observer illuminant] (->spectrum-to-XYZ1 observer illuminant nil))
  ([observer illuminant {:keys [interpolation illuminant-interpolation ^double step extrapolation]
                         :or {interpolation :auto illuminant-interpolation :linear
                              step 1.0 extrapolation :constant}}]
   (when-not (spectrum-extrapolations extrapolation)
     (throw (ex-info (str "Unsupported extrapolation: " extrapolation ", use :constant or :zero")
                     {:extrapolation extrapolation})))
   (let [observer-data (if (map? observer) observer
                           (or (color-matching-functions-data observer)
                               (throw (ex-info (str "Unknown observer: " observer) {:observer observer}))))
         illuminant-data (if (map? illuminant) illuminant
                             (or (illuminants-spectrum-data illuminant)
                                 (throw (ex-info (str "Unknown illuminant: " illuminant) {:illuminant illuminant}))))
         [^double lo ^double hi] (:range observer-data)
         n (m/long-inc (long (m/floor (m/+ m/EPSILON (m// (m/- hi lo) step)))))
         observer-interpolator (fn [k] (spectrum-interpolator :auto :zero
                                                             {:range (:range observer-data)
                                                              :lambda (:lambda observer-data)
                                                              :value (k observer-data)}))
         [x y z] (if (map? observer)
                   [(observer-interpolator :x) (observer-interpolator :y) (observer-interpolator :z)]
                   (cmf observer))
         illuminant-interpolator (spectrum-interpolator illuminant-interpolation extrapolation illuminant-data)
         lambdas (double-array n)
         wx (double-array n) wy (double-array n) wz (double-array n)]
     (dotimes [k n]
       (let [l (m/+ lo (m/* k step))
             s (double (illuminant-interpolator l))]
         (Array/aset lambdas k l)
         (Array/aset wx k (m/* s (double (x l))))
         (Array/aset wy k (m/* s (double (y l))))
         (Array/aset wz k (m/* s (double (z l))))))
     (let [norm (v/sum wy)]
       (fn ^Vec3 [spectrum-data]
         (let [r (spectrum-interpolator interpolation extrapolation spectrum-data)]
           (loop [k (long 0) X 0.0 Y 0.0 Z 0.0]
             (if (m/< k n)
               (let [rv (double (r (Array/aget lambdas k)))]
                 (recur (m/inc k)
                        (m/+ X (m/* rv (Array/aget wx k)))
                        (m/+ Y (m/* rv (Array/aget wy k)))
                        (m/+ Z (m/* rv (Array/aget wz k)))))
               (Vec3. (m// X norm) (m// Y norm) (m// Z norm))))))))))

;; correlated color temperature

(defn daylight-locus
  "Returns the `y` chromaticity coordinate of the CIE daylight locus for a given `x`.

  The CIE daylight locus is `y = -3.000 x^2 + 2.870 x - 0.275`, valid for `x` between about 0.25 and 0.38 (CCT 4000-25000 K).

  See also [[CCT->xy]], [[illuminant-D-spectrum]]."
  ^double [^double x]
  (m/+ (m/* x (m/+ (m/* -3.0 x) 2.87)) -0.275))

(defn CCT->mired
  "Converts a correlated color temperature in K to micro reciprocal degrees (mired), `1e6 / cct`.

  See also [[mired->CCT]]."
  ^double [^double cct]
  (m// 1.0e6 cct))

(defn mired->CCT
  "Converts micro reciprocal degrees (mired) to a correlated color temperature in K, `1e6 / mired`.

  See also [[CCT->mired]]."
  ^double [^double mired]
  (m// 1.0e6 mired))

(defn- unknown-method!
  [method]
  (throw (ex-info (str "Unknown method: " method) {:method method})))

(defn- cct-domain-error
  [method cct lo hi]
  (ex-info (str "Correlated color temperature " cct " K outside the domain of " method ": [" lo ", " hi "]")
           {:method method :cct cct}))

(defn- check-cct-domain!
  [method ^double cct ^double lo ^double hi]
  (when-not (m/<= lo cct hi)
    (throw (cct-domain-error method cct lo hi))))

(def ^{:private true :const true :tag 'double} INVERSE-DOMAIN-RTOL 1.0e-9)

(defn- check-inverse-cct-domain!
  "Domain check of a numerically found CCT: accepts results within the minimiser accuracy of a boundary."
  [method ^double cct ^double lo ^double hi]
  (when-not (m/<= (m/* lo (m/- 1.0 INVERSE-DOMAIN-RTOL)) cct (m/* hi (m/+ 1.0 INVERSE-DOMAIN-RTOL)))
    (throw (cct-domain-error method cct lo hi))))

(defn- CCT->xy-cie-d-unchecked
  ^Vec2 [^double cct]
  (let [x (if (m/<= cct 7000.0)
            (m/+ (m// -4.607e9 (m/* cct cct cct)) (m// 2.9678e6 (m/* cct cct)) (m// 99.11 cct) 0.244063)
            (m/+ (m// -2.0064e9 (m/* cct cct cct)) (m// 1.9018e6 (m/* cct cct)) (m// 247.48 cct) 0.23704))]
    (Vec2. x (daylight-locus x))))

(defn- CCT->xy-cie-d
  ^Vec2 [^double cct]
  (check-cct-domain! :cie-d cct 4000.0 25000.0)
  (CCT->xy-cie-d-unchecked cct))

(defn- CCT->xy-kang2002
  ^Vec2 [^double cct]
  (check-cct-domain! :kang2002 cct 1667.0 25000.0)
  (let [x (if (m/<= cct 4000.0)
            (m/+ (m// -0.2661239e9 (m/* cct cct cct)) (m// -0.2343589e6 (m/* cct cct)) (m// 877.6956 cct) 0.179910)
            (m/+ (m// -3.0258469e9 (m/* cct cct cct)) (m// 2.1070379e6 (m/* cct cct)) (m// 222.6347 cct) 0.24039))
        x2 (m/* x x)
        x3 (m/* x2 x)
        y (cond
            (m/<= cct 2222.0) (m/+ (m/* -1.1063814 x3) (m/* -1.34811020 x2) (m/* 2.18555832 x) -0.20219683)
            (m/<= cct 4000.0) (m/+ (m/* -0.9549476 x3) (m/* -1.37418593 x2) (m/* 2.09137015 x) -0.16748867)
            :else (m/+ (m/* 3.0817580 x3) (m/* -5.8733867 x2) (m/* 3.75112997 x) -0.37001483))]
    (Vec2. x y)))

(defn CCT->xy
  "Converts a correlated color temperature to `x`, `y` chromaticity coordinates.

  Parameters:

  - `method` (keyword, optional):
      - `:cie-d` (default) - CIE daylight (D-series) locus, CCT 4000-25000 K,
      - `:kang2002` - Kang et al. 2002 approximation of the Planckian locus, CCT 1667-25000 K.
  - `cct` (double): correlated color temperature in K.

  Returns `Vec2` `[x y]` (CIE 1931 2° observer).

  Throws `ex-info` with `{:method method}` for an unknown method and `{:method method :cct cct}` for a CCT outside the method's domain.

  See also [[xy->CCT]], [[daylight-locus]], [[illuminant-D-spectrum]]."
  (^Vec2 [^double cct] (CCT->xy-cie-d cct))
  (^Vec2 [method ^double cct]
   (case method
     :cie-d (CCT->xy-cie-d cct)
     :kang2002 (CCT->xy-kang2002 cct)
     (unknown-method! method))))

(defn- minimize-distance
  "CCT in `[lo, hi]` minimising the squared distance between `(cct->coords cct)` and `target`."
  ^double [cct->coords ^Vec2 target ^double lo ^double hi]
  (let [a (.x target)
        b (.y target)
        f (fn ^double [^double cct]
            (let [^Vec2 c (cct->coords cct)
                  da (m/- (.x c) a)
                  db (m/- (.y c) b)]
              (m/+ (m/* da da) (m/* db db))))]
    (first (opt/minimize :brent f {:bounds [[lo hi]] :rel 1.0e-12 :abs 1.0e-10}))))

(defn- xy->CCT-cie-d
  ^double [^double x ^double y]
  ;; search beyond the domain, so that a result outside it is detected instead of clipped
  (let [cct (minimize-distance CCT->xy-cie-d-unchecked (Vec2. x y) 2000.0 50000.0)]
    (check-inverse-cct-domain! :cie-d cct 4000.0 25000.0)
    cct))

(defn- xy->CCT-mccamy
  ^double [^double x ^double y]
  (let [n (m// (m/- x 0.3320) (m/- y 0.1858))]
    (m/+ (m/* n (m/+ (m/* n (m/+ (m/* -449.0 n) 3525.0)) -6823.3)) 5520.33)))

(defn- xy->CCT-hernandez
  ^double [^double x ^double y]
  (let [n (m// (m/- x 0.3366) (m/- y 0.1735))
        cct (m/+ -949.86315
                 (m/* 6253.80338 (m/exp (m// (m/- n) 0.92159)))
                 (m/* 28.70599 (m/exp (m// (m/- n) 0.20039)))
                 (m/* 0.00004 (m/exp (m// (m/- n) 0.07125))))]
    (if (m/> cct 50000.0)
      (let [n (m// (m/- x 0.3356) (m/- y 0.1691))]
        (m/+ 36284.48953
             (m/* 0.00228 (m/exp (m// (m/- n) 0.07861)))
             (m/* 5.4535e-36 (m/exp (m// (m/- n) 0.01543)))))
      cct)))

(defn- xy->CCT-dispatch
  ^double [method ^double x ^double y]
  (case method
    :cie-d (xy->CCT-cie-d x y)
    :mccamy (xy->CCT-mccamy x y)
    :hernandez (xy->CCT-hernandez x y)
    (unknown-method! method)))

(defn xy->CCT
  "Estimates the correlated color temperature of `x`, `y` chromaticity coordinates.

  Parameters:

  - `method` (keyword, optional):
      - `:cie-d` (default) - CCT of the nearest point (in xy) of the CIE daylight locus, found numerically; the result must be in 4000-25000 K (up to a relative 1e-9),
      - `:mccamy` - McCamy 1992 cubic approximation, accurate to about 2 K near the Planckian locus between 2856 K (A) and 6504 K (D65); the error grows outside (about 20 K at 2000 K, 120 K at 10000 K for blackbody chromaticities),
      - `:hernandez` - Hernández-Andrés et al. 1999 exponential approximation, for about 3000-800000 K.
  - `xy` (`Vec2`) or `x`, `y` (doubles): chromaticity coordinates (CIE 1931 2° observer). A 2-arity call with a number as the first argument is `[x y]`, otherwise `[method xy]`.

  Returns the CCT in K as a double. `:mccamy` and `:hernandez` are closed forms and do not check their accuracy range.

  Throws `ex-info` with `{:method method}` for an unknown method and `{:method :cie-d :cct cct}` when the `:cie-d` result is outside its domain.

  See also [[CCT->xy]], [[uv->CCT]]."
  (^double [^Vec2 xy] (xy->CCT-cie-d (.x xy) (.y xy)))
  (^double [a b]
   (if (number? a)
     (xy->CCT-cie-d (double a) (double b))
     (let [^Vec2 xy b] (xy->CCT-dispatch a (.x xy) (.y xy)))))
  (^double [method ^double x ^double y] (xy->CCT-dispatch method x y)))

(defn- CCT->uv-krystek-unchecked
  ^Vec2 [^double t]
  (let [t2 (m/* t t)]
    (Vec2. (m// (m/+ 0.860117757 (m/* 1.54118254e-4 t) (m/* 1.28641212e-7 t2))
                (m/+ 1.0 (m/* 8.42420235e-4 t) (m/* 7.08145163e-7 t2)))
           (m// (m/+ 0.317398726 (m/* 4.22806245e-5 t) (m/* 4.20481691e-8 t2))
                (m/+ 1.0 (m/* -2.89741816e-5 t) (m/* 1.61456053e-7 t2))))))

(defn CCT->uv
  "Converts a correlated color temperature to CIE 1960 UCS `u`, `v` coordinates of the Planckian locus.

  Parameters:

  - `method` (keyword, optional): `:krystek` (default) - Krystek 1985 rational approximation, CCT 1000-15000 K.
  - `cct` (double): correlated color temperature in K.

  Returns `Vec2` `[u v]`.

  Throws `ex-info` with `{:method method}` for an unknown method and `{:method method :cct cct}` for a CCT outside the domain.

  See also [[uv->CCT]], [[CCT->xy]]."
  (^Vec2 [^double cct] (CCT->uv :krystek cct))
  (^Vec2 [method ^double cct]
   (case method
     :krystek (do (check-cct-domain! :krystek cct 1000.0 15000.0)
                  (CCT->uv-krystek-unchecked cct))
     (unknown-method! method))))

(defn- uv->CCT-krystek
  ^double [^double u ^double v]
  (let [cct (minimize-distance CCT->uv-krystek-unchecked (Vec2. u v) 500.0 30000.0)]
    (check-inverse-cct-domain! :krystek cct 1000.0 15000.0)
    cct))

(defn uv->CCT
  "Estimates the correlated color temperature of CIE 1960 UCS `u`, `v` coordinates.

  Parameters:

  - `method` (keyword, optional): `:krystek` (default) - CCT of the nearest point (in uv) of the Krystek 1985 Planckian locus, found numerically; the result must be in 1000-15000 K (up to a relative 1e-9).
  - `uv` (`Vec2`) or `u`, `v` (doubles): CIE 1960 UCS coordinates. A 2-arity call with a number as the first argument is `[u v]`, otherwise `[method uv]`.

  Returns the CCT in K as a double.

  Throws `ex-info` with `{:method method}` for an unknown method and `{:method :krystek :cct cct}` when the result is outside the domain.

  See also [[CCT->uv]], [[xy->CCT]]."
  (^double [^Vec2 uv] (uv->CCT-krystek (.x uv) (.y uv)))
  (^double [a b]
   (if (number? a)
     (uv->CCT-krystek (double a) (double b))
     (let [^Vec2 uv b] (uv->CCT a (.x uv) (.y uv)))))
  (^double [method ^double u ^double v]
   (case method
     :krystek (uv->CCT-krystek u v)
     (unknown-method! method))))

;; spectra of light sources

(def ^{:private true :const true :tag 'double} PLANCK-C1 3.741771e-16) ;; W m^2
(def ^{:private true :const true :tag 'double} PLANCK-C2 1.4388e-2) ;; m K, as ITS-90 and CIE 15

(defn planck-law
  "Spectral radiance of a blackbody (Planck's law).

  Uses the radiation constants `c1 = 3.741771e-16 W m^2` and `c2 = 1.4388e-2 m K` (the value of ITS-90 and CIE 15), in vacuum.

  Parameters:

  - `lambda` (double): wavelength in nm, positive.
  - `temperature` (double): temperature in K, positive.

  Returns the spectral radiance in W sr^-1 m^-2 nm^-1.

  See also [[blackbody-spectrum]]."
  ^double [^double lambda ^double temperature]
  (let [l (m/* lambda 1.0e-9)]
    (m/* 1.0e-9 (m// PLANCK-C1 (m/* m/PI (m/pow l 5.0) (m/expm1 (m// PLANCK-C2 (m/* l temperature))))))))

(def ^:private default-wavelengths (range 360 781))

(defn blackbody-spectrum
  "Creates the spectrum of a blackbody radiator from [[planck-law]].

  Parameters:

  - `temperature` (double): temperature in K.
  - `lambdas` (sequence of numbers, optional): wavelengths in nm, default 360-780 nm every 1 nm.

  Returns a spectrum map (see [[spectrum]]) with spectral radiance values in W sr^-1 m^-2 nm^-1, usable as an illuminant of [[->spectrum-to-XYZ1]].

  See also [[illuminant-A-spectrum]], [[illuminant-D-spectrum]]."
  ([^double temperature] (blackbody-spectrum temperature default-wavelengths))
  ([^double temperature lambdas]
   (spectrum lambdas (mapv (fn [^double l] (planck-law l temperature)) lambdas))))

(def ^{:private true :const true :tag 'double} ILLUMINANT-A-C2 1.435e7) ;; nm K, CIE definition of A
(def ^{:private true :const true :tag 'double} ILLUMINANT-A-T 2848.0)

(defn illuminant-A-spectrum
  "Creates the spectrum of CIE standard illuminant A from its closed form definition (CIE 15).

  `S(λ) = 100 (560 / λ)^5 (exp(c2 / (2848 · 560)) - 1) / (exp(c2 / (2848 λ)) - 1)` with `c2 = 1.435e7 nm K`, normalised to 100 at 560 nm.

  Parameters:

  - `lambdas` (sequence of numbers, optional): wavelengths in nm, default 360-780 nm every 1 nm.

  Returns a spectrum map (see [[spectrum]]).

  See also [[blackbody-spectrum]], [[illuminants-spectrum-data]] (tabulated `:A`)."
  ([] (illuminant-A-spectrum default-wavelengths))
  ([lambdas]
   (let [n (m/expm1 (m// ILLUMINANT-A-C2 (m/* ILLUMINANT-A-T 560.0)))]
     (spectrum lambdas (mapv (fn [^double l] (m/* 100.0 (m/pow (m// 560.0 l) 5.0)
                                                 (m// n (m/expm1 (m// ILLUMINANT-A-C2 (m/* ILLUMINANT-A-T l))))))
                             lambdas)))))

(def ^:private d-series-basis (delay (read-edn "color/illuminant_d_series_basis.edn")))

(defn- round-3
  "Rounds to 3 decimals, half to even (as CIE practice and numpy)."
  ^double [^double v]
  (m// (m/rint (m/* v 1000.0)) 1000.0))

(defn illuminant-D-spectrum
  "Creates the spectrum of a CIE D-series (daylight) illuminant.

  The spectrum is `S0 + M1 S1 + M2 S2` with the CIE basis functions (300-830 nm, 5 nm) and `M1`, `M2` computed from the chromaticity and rounded to 3 decimals, as CIE 15 prescribes.

  Parameters:

  - `xy` (`Vec2`): chromaticity coordinates of the illuminant, or
  - `cct` (double): correlated color temperature in K (4000-25000), converted with [[CCT->xy]] `:cie-d`. The nominal CCT of D65 is 6500 · 1.4388 / 1.4380 (about 6504 K), of D50 5000 · 1.4388 / 1.4380.

  Returns a spectrum map (see [[spectrum]]) at 300-830 nm every 5 nm, usable as an illuminant of [[->spectrum-to-XYZ1]].

  Throws `ex-info` (see [[CCT->xy]]) for a CCT outside 4000-25000 K.

  See also [[daylight-locus]], [[illuminant-A-spectrum]]."
  [xy-or-cct]
  (let [^Vec2 xy (if (number? xy-or-cct) (CCT->xy :cie-d xy-or-cct) xy-or-cct)
        x (.x xy)
        y (.y xy)
        mm (m/+ 0.0241 (m/* 0.2562 x) (m/* -0.7341 y))
        m1 (round-3 (m// (m/+ -1.3515 (m/* -1.7703 x) (m/* 5.9114 y)) mm))
        m2 (round-3 (m// (m/+ 0.0300 (m/* -31.4424 x) (m/* 30.0717 y)) mm))
        {:keys [lambda S0 S1 S2]} @d-series-basis]
    (spectrum lambda (mapv (fn [^double s0 ^double s1 ^double s2] (m/+ s0 (m/* m1 s1) (m/* m2 s2))) S0 S1 S2))))

