(ns clojure2d.color.rgb
  "RGB color space definitions and their transfer functions, used by the RGB conversions of [[clojure2d.color]].

  An RGB space is a map (see [[rgbs]] for the predefined ones, [[rgb-data->map]] to build one) with keys:

  - `:red`, `:green`, `:blue` - `[x y]` chromaticity coordinates of the primaries,
  - `:whitepoint` - white point `Vec4` from `clojure2d.color.whitepoints`,
  - `:rgb-to-xyz`, `:xyz-to-rgb` - `Mat3x3` matrices between linear RGB (0-1) and XYZ (`Y` 0-1),
  - `:to` - transfer function encoding a linear channel value to the space's encoded value,
  - `:from` - transfer function decoding an encoded value to a linear one.

  Transfer functions work on single channel values, linear and encoded values both nominally in 0-1 (values outside are accepted). Encoding functions are named `to-...` (and `linear-to-srgb`), decoding functions `from-...` (and `srgb-to-linear`):

  - sRGB: [[linear-to-srgb]], [[srgb-to-linear]],
  - power law: [[->gamma]],
  - CIE L*: [[to-L*]], [[from-L*]],
  - ACES log: [[to-ACEScc]], [[from-ACEScc]], [[to-ACEScct]], [[from-ACEScct]],
  - Blackmagic Film Generation 5: [[to-Blackmagic-WG]], [[from-Blackmagic-WG]],
  - ITU-R BT.2020: [[to-BT2020]], [[from-BT2020]],
  - ITU-R BT.709: [[to-BT709]], [[from-BT709]],
  - ROMM RGB / ProPhoto RGB: [[to-ROMM]], [[from-ROMM]],
  - SMPTE 240M: [[to-SMPTE240M]], [[from-SMPTE240M]],
  - ITU-R BT.1886 display EOTF: [[to-BT1886]], [[from-BT1886]],
  - HDR: SMPTE ST 2084 PQ [[to-PQ]], [[from-PQ]] and ITU-R BT.2100 HLG [[to-HLG]], [[from-HLG]]."
  (:require [clojure2d.color.whitepoints :as wp]
            [fastmath.matrix :as mat]
            [fastmath.core :as m])
  (:import [fastmath.matrix Mat3x3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn srgb-to-linear
  "Decodes an sRGB encoded channel value to a linear one (sRGB EOTF, IEC 61966-2-1).

  `v / 12.92` for `v <= 0.040449936`, `((v + 0.055) / 1.055)^2.4` otherwise. The threshold is the intersection of both segments; the standard's 0.04045 differs by less than 3e-9 in the result. Negative values use the linear segment.

  See also [[linear-to-srgb]]."
  ^double [^double v]
  (if (m/> v 0.040449936)
    (m/pow (m// (m/+ 0.055 v) 1.055) 2.4)
    (m// v 12.92)))

(defn linear-to-srgb
  "Encodes a linear channel value to sRGB (inverse sRGB EOTF, IEC 61966-2-1).

  `12.92 v` for `v <= 0.0031308`, `1.055 v^(1/2.4) - 0.055` otherwise. Negative values use the linear segment.

  See also [[srgb-to-linear]]."
  ^double [^double v]
  (if (m/> v 0.0031308)
    (m/- (m/* 1.055 (m/pow v 0.4166666666666667)) 0.055)
    (m/* v 12.92)))

(defn ->gamma
  "Creates a power law transfer function `x^e`, symmetric around 0 (`-(-x)^e` for negative `x`).

  Parameters:

  - `e` (double): exponent; `1/γ` encodes, `γ` decodes a gamma `γ` space, for example `(->gamma (/ 2.2))` and `(->gamma 2.2)`.

  Returns a function of one double.

  See also [[rgbs]]."
  [^double e]
  (fn ^double [^double x]
    (m/spow x e)))

;; memoized version
(def ^:private ->mgamma (memoize ->gamma))

(def ^{:private true :const true :tag 'double} CIEEpsilon (/ 216.0 24389.0))
(def ^{:private true :const true :tag 'double} CIEK (/ 24389.0 27.0))

(defn to-L*
  "Encodes a linear channel value with the CIE L* curve, scaled to 0-1 (as ECI RGB v2).

  `(116 cbrt(v) - 16) / 100` for `v > 216/24389`, `v · 24389/27 / 100` otherwise (also for negative `v`).

  See also [[from-L*]]."
  ^double [^double v]
  (m// (if (> v CIEEpsilon)
         (m/- (m/* 116.0 (m/cbrt v)) 16.0)
         (* v CIEK)) 100.0))

(defn from-L*
  "Decodes a CIE L* encoded channel value (0-1) to a linear one; inverse of [[to-L*]]."
  ^double [^double v]
  (let [v (m// (m/+ (m/* v 100.0) 16.0) 116.0)
        v3 (* v v v)]
    (if (> v3 CIEEpsilon)
      v3
      (/ (- (* 116.0 v) 16.0) CIEK))))

(defn to-ACEScc
  "Encodes a linear (ACES AP1) channel value with the ACEScc logarithmic curve (Academy S-2014-003).

  `(log2(v) + 9.72) / 17.52` for `v >= 2^-15`, `(log2(2^-16 + v/2) + 9.72) / 17.52` below, and the constant `(-16 + 9.72) / 17.52` for `v <= 0`, so non-positive values do not round-trip.

  See also [[from-ACEScc]], [[to-ACEScct]]."
  ^double [^double v]
  (cond
    (m/not-pos? v) -0.35844748858447484
    (m/>= v 3.0517578125E-5) (m// (m/+ (m/log2 v) 9.72) 17.52)
    :else (m// (m/+ (m/log2 (m/+ 1.52587890625E-5 (m/* 0.5 v))) 9.72) 17.52)))

(defn from-ACEScc
  "Decodes an ACEScc channel value to a linear one (Academy S-2014-003); results are capped at 65504 (the largest half float), reached at `(log2(65504) + 9.72) / 17.52`.

  See also [[to-ACEScc]]."
  ^double [^double v]
  (cond
    (m/<= v -0.3013698630136986) (m/* 2.0 (m/- (m/pow 2.0 (m/- (m/* v 17.52) 9.72)) 1.52587890625E-5))
    (m/>= v 1.4679963120447153) 65504.0
    :else (m/pow 2.0 (m/- (m/* v 17.52) 9.72))))

(defn to-ACEScct
  "Encodes a linear (ACES AP1) channel value with the ACEScct curve (Academy S-2016-001): the ACEScc logarithm above `2^-7`, a linear toe `10.5402377416545 v + 0.0729055341958355` below (also for negative `v`).

  See also [[from-ACEScct]], [[to-ACEScc]]."
  ^double [^double v]
  (if (m/<= v 0.0078125)
    (m/+ (m/* v 10.5402377416545) 0.0729055341958355)
    (m// (m/+ (m/log2 v) 9.72) 17.52)))

(defn from-ACEScct
  "Decodes an ACEScct channel value to a linear one (Academy S-2016-001); results are capped at 65504 (the largest half float), as the ACES documentation specifies.

  See also [[to-ACEScct]]."
  ^double [^double v]
  (cond
    (m/<= v 0.155251141552511) (m// (m/- v 0.0729055341958355) 10.5402377416545)
    (m/>= v 1.4679963120447153) 65504.0
    :else (m/pow 2.0 (m/- (m/* v 17.52) 9.72))))

(defn to-Blackmagic-WG
  "Encodes a linear channel value with the Blackmagic Film Generation 5 log curve (the transfer function of Blackmagic Wide Gamut).

  Linear segment `8.283605932402494 v + 0.09246575342465753` below `v = 0.005`, logarithm above.

  See also [[from-Blackmagic-WG]]."
  ^double [^double v]
  (if (m/< v 0.005)
    (m/+ (m/* v 8.283605932402494) 0.09246575342465753)
    (m/+ 0.5300133392291939 (m/* 0.08692876065491224 (m/log (m/+ v 0.005494072432257808))))))

(defn from-Blackmagic-WG
  "Decodes a Blackmagic Film Generation 5 log value to a linear one; inverse of [[to-Blackmagic-WG]]. Large inputs overflow to `##Inf`."
  ^double [^double v]
  (if (m/< v 0.13388378308667)
    (m// (m/- v 0.09246575342465753) 8.283605932402494)
    (m/- (m/exp (m// (m/- v 0.5300133392291939) 0.08692876065491224)) 0.005494072432257808)))

(defn to-BT2020
  "Encodes a linear channel value with the ITU-R BT.2020 OETF, with the double precision constants `α = 1.09929682680944`, `β = 0.018053968510807`.

  `4.5 v` for `v < β`, `α v^0.45 - (α - 1)` otherwise.

  See also [[from-BT2020]]."
  ^double [^double v]
  (if (m/< v 0.018053968510807)
    (m/* 4.5 v)
    (m/- (m/* 1.09929682680944 (m/spow v 0.45)) 0.09929682680944)))

(defn from-BT2020
  "Decodes an ITU-R BT.2020 encoded value to a linear one; inverse of [[to-BT2020]] (inverse OETF)."
  ^double [^double v]
  (if (m/< v 0.08124285829863397)
    (m// v 4.5)
    (m/spow (m// (m/+ v 0.09929682680944) 1.09929682680944) 2.2222222222222222)))

;; ITU-R BT.709 (= BT.601)

;; decoding switches segments at the encoded value of the break point, as colour-science
(def ^{:private true :const true :tag 'double} BT709-V-BREAK (m/- (m/* 1.099 (m/pow 0.018 0.45)) 0.099))

(defn to-BT709
  "Encodes a linear channel value with the ITU-R BT.709 OETF: `4.5 v` for `v < 0.018`, `1.099 v^0.45 - 0.099` otherwise (the standard's rounded constants).

  The OETF is the camera (scene referred) encoding; displays decode BT.709 video with the BT.1886 EOTF ([[from-BT1886]], gamma 2.4), so OETF and EOTF are not inverses of each other.

  See also [[from-BT709]], [[to-BT2020]]."
  ^double [^double v]
  (if (m/< v 0.018)
    (m/* 4.5 v)
    (m/- (m/* 1.099 (m/spow v 0.45)) 0.099)))

(defn from-BT709
  "Decodes an ITU-R BT.709 encoded value to a linear one; inverse of [[to-BT709]] (inverse OETF).

  The segments switch at `to-BT709(0.018)` (about 0.08124), as colour-science. With the standard's rounded constants the segments do not meet exactly, so the function is not monotonic just below the switch: `0.0812` decodes to 0.018044, `0.08125` to 0.018000."
  ^double [^double v]
  (if (m/< v BT709-V-BREAK)
    (m// v 4.5)
    (m/spow (m// (m/+ v 0.099) 1.099) (m// 0.45))))

;; ROMM RGB / ProPhoto RGB

(def ^{:private true :const true :tag 'double} ROMM-ET (/ 512.0))

(defn to-ROMM
  "Encodes a linear channel value with the ROMM RGB (ProPhoto RGB) curve: `16 v` below `1/512`, `v^(1/1.8)` otherwise (ISO 22028-2, floating point form).

  See also [[from-ROMM]]."
  ^double [^double v]
  (if (m/< v ROMM-ET)
    (m/* 16.0 v)
    (m/spow v (m// 1.8))))

(defn from-ROMM
  "Decodes a ROMM RGB (ProPhoto RGB) encoded value to a linear one; inverse of [[to-ROMM]]."
  ^double [^double v]
  (if (m/< v (m/* 16.0 ROMM-ET))
    (m// v 16.0)
    (m/spow v 1.8)))

;; SMPTE 240M

(def ^{:private true :const true :tag 'double} SMPTE240M-V-BREAK (m/- (m/* 1.1115 (m/pow 0.0228 0.45)) 0.1115))

(defn to-SMPTE240M
  "Encodes a linear channel value with the SMPTE 240M OETF: `4 v` for `v < 0.0228`, `1.1115 v^0.45 - 0.1115` otherwise.

  See also [[from-SMPTE240M]]."
  ^double [^double v]
  (if (m/< v 0.0228)
    (m/* 4.0 v)
    (m/- (m/* 1.1115 (m/spow v 0.45)) 0.1115)))

(defn from-SMPTE240M
  "Decodes a SMPTE 240M encoded value to a linear one (SMPTE 240M EOTF); inverse of [[to-SMPTE240M]]."
  ^double [^double v]
  (if (m/< v SMPTE240M-V-BREAK)
    (m// v 4.0)
    (m/spow (m// (m/+ v 0.1115) 1.1115) (m// 0.45))))

;; ITU-R BT.1886

(defn from-BT1886
  "Decodes a video signal value with the ITU-R BT.1886 reference EOTF (gamma 2.4) to display luminance.

  Parameters:

  - `v` (double): encoded value, nominally 0-1.
  - `lw`, `lb` (doubles, optional): display white and black luminance, `lw > lb >= 0`; default 1 and 0, where the EOTF is the plain power `v^2.4`. Other values give `##NaN` or infinities.

  Returns `a · max(v + b, 0)^2.4` with `a = (lw^(1/2.4) - lb^(1/2.4))^2.4`, `b = lb^(1/2.4) / (lw^(1/2.4) - lb^(1/2.4))`; negative arguments of the power are clamped to 0.

  See also [[to-BT1886]]."
  (^double [^double v] (from-BT1886 v 1.0 0.0))
  (^double [^double v ^double lw ^double lb]
   (let [n (m/- (m/pow lw (m// 2.4)) (m/pow lb (m// 2.4)))
         a (m/pow n 2.4)
         b (m// (m/pow lb (m// 2.4)) n)]
     (m/* a (m/spow (m/max (m/+ v b) 0.0) 2.4)))))

(defn to-BT1886
  "Encodes display luminance with the inverse ITU-R BT.1886 EOTF; inverse of [[from-BT1886]] with the same optional `lw`, `lb` (`lw > lb >= 0`, default 1 and 0, the plain power `v^(1/2.4)`)."
  (^double [^double v] (to-BT1886 v 1.0 0.0))
  (^double [^double v ^double lw ^double lb]
   (let [n (m/- (m/pow lw (m// 2.4)) (m/pow lb (m// 2.4)))
         a (m/pow n 2.4)
         b (m// (m/pow lb (m// 2.4)) n)]
     (m/- (m/spow (m// v a) (m// 2.4)) b))))

;; SMPTE ST 2084 (PQ)

(def ^{:private true :const true :tag 'double} PQ-M1 0.1593017578125)
(def ^{:private true :const true :tag 'double} PQ-M2 78.84375)
(def ^{:private true :const true :tag 'double} PQ-C1 0.8359375)
(def ^{:private true :const true :tag 'double} PQ-C2 18.8515625)
(def ^{:private true :const true :tag 'double} PQ-C3 18.6875)

(defn to-PQ
  "Encodes linear light with the SMPTE ST 2084 (PQ) inverse EOTF, the HDR10 / ITU-R BT.2100 PQ curve.

  The input is display luminance normalised to 10000 cd/m² (`1.0` = 10000 cd/m², `0.01` = 100 cd/m²); the result is the PQ signal in 0-1.

  See also [[from-PQ]], [[to-HLG]]."
  ^double [^double v]
  (let [y (m/spow v PQ-M1)]
    (m/spow (m// (m/+ PQ-C1 (m/* PQ-C2 y)) (m/+ (m/* PQ-C3 y) 1.0)) PQ-M2)))

(defn from-PQ
  "Decodes a SMPTE ST 2084 (PQ) signal to linear light normalised to 10000 cd/m² (PQ EOTF); inverse of [[to-PQ]], non-positive signals give 0."
  ^double [^double v]
  (let [vp (m/spow v (m// PQ-M2))
        n (m/max 0.0 (m/- vp PQ-C1))]
    (m/spow (m// n (m/- PQ-C2 (m/* PQ-C3 vp))) (m// PQ-M1))))

;; ITU-R BT.2100 HLG

(def ^{:private true :const true :tag 'double} HLG-A 0.17883277)
(def ^{:private true :const true :tag 'double} HLG-B 0.28466892)
(def ^{:private true :const true :tag 'double} HLG-C 0.559910729529562)

(defn to-HLG
  "Encodes scene linear light (0-1) with the ITU-R BT.2100 hybrid log-gamma (HLG) OETF: `sqrt(3 v)` for `v <= 1/12` (symmetric for negative `v`), `a ln(12 v - b) + c` above, with `a = 0.17883277`, `b = 1 - 4a`, `c = 0.5 - a ln(4a)`.

  Scene referred: the HLG OOTF (system gamma) is not applied.

  See also [[from-HLG]], [[to-PQ]]."
  ^double [^double v]
  (let [e (m/* 12.0 v)]
    (if (m/<= e 1.0)
      (m/* 0.5 (m/spow e 0.5))
      (m/+ (m/* HLG-A (m/log (m/- e HLG-B))) HLG-C))))

(defn from-HLG
  "Decodes an ITU-R BT.2100 HLG signal to scene linear light (inverse OETF); inverse of [[to-HLG]]."
  ^double [^double v]
  (m// (if (m/<= v 0.5)
         (m/spow (m/* 2.0 v) 2.0)
         (m/+ (m/exp (m// (m/- v HLG-C) HLG-A)) HLG-B))
       12.0))

;;

(defn rgb-data->matrix
  "Calculates the matrices between linear RGB and XYZ of an RGB space from its primaries and white point.

  The RGB to XYZ matrix maps the primaries to their chromaticities and RGB `[1 1 1]` to the white point tristimulus values (`Y = 1`, see `clojure2d.color.whitepoints/tristimulus`).

  Parameters:

  - `red`, `green`, `blue` (`[x y]` each): chromaticity coordinates of the primaries.
  - `whitepoint` (`Vec4`): white point from `clojure2d.color.whitepoints`.

  Returns a pair `[rgb-to-xyz xyz-to-rgb]` of `Mat3x3`.

  See also [[rgb-data->map]]."
  [[^double red-x ^double red-y]
   [^double green-x ^double green-y]
   [^double blue-x ^double blue-y]
   whitepoint]
  (let [rgbt (Mat3x3. red-x   green-x   blue-x
                      red-y   green-y   blue-y
                      (m/- 1.0 red-x red-y) (m/- 1.0 green-x green-y)  (m/- 1.0 blue-x blue-y))
        wp (wp/tristimulus whitepoint)
        coeffs (apply mat/mat3x3 (mat/mulv (mat/inverse rgbt) wp))
        m (mat/mulm rgbt coeffs)]
    [m (mat/inverse m)]))

(defn rgb-data->map
  "Builds an RGB space map (see the namespace docstring for its keys) from primaries, white point and transfer functions.

  Parameters:

  - `red`, `green`, `blue` (`[x y]` each): chromaticity coordinates of the primaries.
  - `whitepoint` (`Vec4`): white point from `clojure2d.color.whitepoints`.
  - `to` (function of one double): encoding transfer function, linear to encoded.
  - `from` (function of one double): decoding transfer function, encoded to linear.

  Returns a map with `:red`, `:green`, `:blue`, `:whitepoint`, `:to`, `:from` and the matrices `:rgb-to-xyz`, `:xyz-to-rgb` from [[rgb-data->matrix]]. The map can be used wherever a space name is accepted, for example in `clojure2d.color/->RGB-to-RGB`.

  See also [[rgbs]]."
  [red green blue whitepoint to from]
  (let [[m1 m2] (rgb-data->matrix red green blue whitepoint)]
    {:red red :green green :blue blue
     :rgb-to-xyz m1
     :xyz-to-rgb m2
     :whitepoint whitepoint
     :to to
     :from from}))

(def ^{:doc "Predefined RGB spaces, a map from keyword to RGB space map (see the namespace docstring for the keys).

  Spaces (white point var of `clojure2d.color.whitepoints`, transfer function):

  - `:ACES2065-1`, `:ACEScg` - ACES AP0 and AP1 primaries, `CIE-2-ACES`, linear,
  - `:ACEScc`, `:ACEScct` - ACES AP1, `CIE-2-ACES`, [[to-ACEScc]], [[to-ACEScct]],
  - `:Adobe-RGB-1998` - `CIE-2-D65`, gamma 563/256,
  - `:Adobe-Wide-Gamut-RGB` - `CIE-2-D50`, gamma 563/256,
  - `:Apple-RGB` - `CIE-2-D65`, gamma 1.8,
  - `:Best-RGB`, `:Beta-RGB`, `:Don-RGB`, `:Ekta-Space-PS-5`, `:Xtreme-RGB` - `CIE-2-D50`, gamma 2.2,
  - `:Blackmagic-Wide-Gamut` - `CIE-2-BLACKMAGIC-WG`, [[to-Blackmagic-WG]] (Film Generation 5),
  - `:Canon-Cinema-Gamut` - `CIE-2-D65`, linear,
  - `:CIE-RGB` - `CIE-2-E`, gamma 2.2,
  - `:ColorMatch-RGB` - `CIE-2-D50`, gamma 1.8,
  - `:ECI-RGB-v2` - `CIE-2-D50`, CIE L* ([[to-L*]]),
  - `:ITU-R-BT2020` - `CIE-2-D65`, [[to-BT2020]],
  - `:NTSC` (alias `:ITU-R-BT470-525`) - NTSC 1953 primaries, `CIE-2-C`, gamma 2.2 (ITU-R BT.470 System M),
  - `:NTSC-colour` - as `:NTSC` with gamma 2.8, the value of colour-science, kept for compatibility,
  - `:PAL-SECAM` (alias `:ITU-R-BT470-625`) - `CIE-2-D65`, gamma 2.8,
  - `:sRGB` - `CIE-2-D65`, [[linear-to-srgb]]; its matrices are derived from the primaries and differ by at most 2.2e-6 from the fixed (Lindbloom) matrices of `clojure2d.color/to-XYZ` and `from-XYZ`, which keeps white mapped exactly to white between D65 spaces,
  - `:SMPTE-C` - `CIE-2-D65`, gamma 2.2,
  - `:ITU-R-BT709` - sRGB primaries, `CIE-2-D65`, [[to-BT709]] (camera OETF, scene referred, as colour-science: the same light gives other code values than sRGB, for example sRGB 128 becomes 115.3; for display referred BT.709 use [[rgb-data->map]] with [[to-BT1886]], [[from-BT1886]]),
  - `:Display-P3` - P3 primaries, `CIE-2-D65`, [[linear-to-srgb]],
  - `:P3-D65` - P3 primaries, `CIE-2-D65`, gamma 2.6,
  - `:DCI-P3` - P3 primaries, `CIE-2-DCI-P3`, gamma 2.6,
  - `:SMPTE-240M` - `CIE-2-D65`, [[to-SMPTE240M]],
  - `:ProPhoto-RGB` (alias `:ROMM-RGB`) - `CIE-2-D50`, [[to-ROMM]].

  Primaries as in colour-science; white points are the full precision values of `clojure2d.color.whitepoints`, so matrices differ from those built with rounded white points (for example D65 0.3127, 0.3290) by up to about 2.4e-4.

  See also [[rgb-space]], [[rgb-spaces-list]], [[rgb-data->map]]."}
  rgbs (let [m {:ACES2065-1 (rgb-data->map   [0.73470, 0.26530],
                                              [0.00000, 1.00000],
                                              [0.00010, -0.07700]
                                              wp/CIE-2-ACES
                                              identity identity)
                 :ACEScg (rgb-data->map  [0.71300, 0.29300],
                                         [0.16500, 0.83000],
                                         [0.12800, 0.04400]
                                         wp/CIE-2-ACES
                                         identity identity)
                 :ACEScc (rgb-data->map  [0.71300, 0.29300],
                                         [0.16500, 0.83000],
                                         [0.12800, 0.04400]
                                         wp/CIE-2-ACES
                                         to-ACEScc from-ACEScc)
                 :ACEScct (rgb-data->map  [0.71300, 0.29300],
                                          [0.16500, 0.83000],
                                          [0.12800, 0.04400]
                                          wp/CIE-2-ACES
                                          to-ACEScct from-ACEScct)
                 :Adobe-RGB-1998 (rgb-data->map [0.6400, 0.3300]
                                                [0.2100, 0.7100]
                                                [0.1500, 0.0600]
                                                wp/CIE-2-D65                                         
                                                (->mgamma 0.4547069271758437)
                                                (->mgamma 2.19921875))
                 :Adobe-Wide-Gamut-RGB (rgb-data->map [0.7347, 0.2653],
                                                      [0.1152, 0.8264],
                                                      [0.1566, 0.0177]
                                                      wp/CIE-2-D50
                                                      (->mgamma 0.4547069271758437)
                                                      (->mgamma 2.19921875))
                 :Apple-RGB (rgb-data->map [0.6250, 0.3400],
                                           [0.2800, 0.5950],
                                           [0.1550, 0.0700]
                                           wp/CIE-2-D65
                                           (->mgamma 0.5555555555555556)
                                           (->mgamma 1.8))
                 :Best-RGB (rgb-data->map  [0.735191637630662, 0.264808362369338],
                                           [0.215336134453781, 0.774159663865546],
                                           [0.130122950819672, 0.034836065573770]
                                           wp/CIE-2-D50
                                           (->mgamma 0.45454545454545453)
                                           (->mgamma 2.2))
                 :Beta-RGB (rgb-data->map  [0.6888, 0.3112],
                                           [0.1986, 0.7551],
                                           [0.1265, 0.0352]
                                           wp/CIE-2-D50
                                           (->mgamma 0.45454545454545453)
                                           (->mgamma 2.2))
                 :Blackmagic-Wide-Gamut (rgb-data->map  [0.7177215, 0.3171181],
                                                        [0.2280410, 0.8615690],
                                                        [0.1005841, -0.0820452]
                                                        wp/CIE-2-BLACKMAGIC-WG
                                                        to-Blackmagic-WG from-Blackmagic-WG)
                 :Canon-Cinema-Gamut (rgb-data->map [0.7400, 0.2700],
                                                    [0.1700, 1.1400],
                                                    [0.0800, -0.1000]
                                                    wp/CIE-2-D65
                                                    identity identity)
                 :CIE-RGB (rgb-data->map [0.734742840005998, 0.265257159994002],
                                         [0.273779033824958, 0.717477700256116],
                                         [0.166555629580280, 0.008910726182545]
                                         wp/CIE-2-E
                                         (->mgamma 0.45454545454545453)
                                         (->mgamma 2.2))
                 :ColorMatch-RGB (rgb-data->map [0.6300, 0.3400],
                                                [0.2950, 0.6050],
                                                [0.1500, 0.0750]
                                                wp/CIE-2-D50
                                                (->mgamma 0.5555555555555556)
                                                (->mgamma 1.8))
                 :Don-RGB (rgb-data->map [0.696120689655172, 0.299568965517241],
                                         [0.214682981090100, 0.765294771968854],
                                         [0.129937629937630, 0.035343035343035]
                                         wp/CIE-2-D50
                                         (->mgamma 0.45454545454545453)
                                         (->mgamma 2.2))
                 :Ekta-Space-PS-5 (rgb-data->map [0.694736842105263, 0.305263157894737],
                                                 [0.260000000000000, 0.700000000000000],
                                                 [0.109728506787330, 0.004524886877828]
                                                 wp/CIE-2-D50
                                                 (->mgamma 0.45454545454545453)
                                                 (->mgamma 2.2))
                 :ECI-RGB-v2 (rgb-data->map [0.670103092783505, 0.329896907216495],
                                            [0.209905660377358, 0.709905660377358],
                                            [0.140061791967044, 0.080329557157570]
                                            wp/CIE-2-D50
                                            to-L* from-L*)
                 :ITU-R-BT2020 (rgb-data->map [0.7080, 0.2920],
                                              [0.1700, 0.7970],
                                              [0.1310, 0.0460]
                                              wp/CIE-2-D65
                                              to-BT2020 from-BT2020)
                 :NTSC (rgb-data->map [0.6700, 0.3300],
                                      [0.2100, 0.7100],
                                      [0.1400, 0.0800]
                                      wp/CIE-2-C
                                      (->mgamma 0.45454545454545453)
                                      (->mgamma 2.2))
                 :NTSC-colour (rgb-data->map [0.6700, 0.3300],
                                             [0.2100, 0.7100],
                                             [0.1400, 0.0800]
                                             wp/CIE-2-C
                                             (->mgamma 0.35714285714285715)
                                             (->mgamma 2.8))
                 :PAL-SECAM (rgb-data->map [0.6400, 0.3300],
                                           [0.2900, 0.6000],
                                           [0.1500, 0.0600] 
                                           wp/CIE-2-D65
                                           (->mgamma 0.35714285714285715)
                                           (->mgamma 2.8))
                 :sRGB (rgb-data->map [0.6400, 0.3300]
                                      [0.3000, 0.6000]
                                      [0.1500, 0.0600]
                                      wp/CIE-2-D65
                                      linear-to-srgb srgb-to-linear)
                 :SMPTE-C (rgb-data->map [0.630, 0.340],
                                         [0.310, 0.595],
                                         [0.155, 0.070]
                                         wp/CIE-2-D65
                                         (->mgamma 0.45454545454545453)
                                         (->mgamma 2.2))
                 :ITU-R-BT709 (rgb-data->map [0.6400, 0.3300]
                                             [0.3000, 0.6000]
                                             [0.1500, 0.0600]
                                             wp/CIE-2-D65
                                             to-BT709 from-BT709)
                 :Display-P3 (rgb-data->map [0.680, 0.320]
                                            [0.265, 0.690]
                                            [0.150, 0.060]
                                            wp/CIE-2-D65
                                            linear-to-srgb srgb-to-linear)
                 :P3-D65 (rgb-data->map [0.680, 0.320]
                                        [0.265, 0.690]
                                        [0.150, 0.060]
                                        wp/CIE-2-D65
                                        (->mgamma (/ 2.6))
                                        (->mgamma 2.6))
                 :DCI-P3 (rgb-data->map [0.680, 0.320]
                                        [0.265, 0.690]
                                        [0.150, 0.060]
                                        wp/CIE-2-DCI-P3
                                        (->mgamma (/ 2.6))
                                        (->mgamma 2.6))
                 :SMPTE-240M (rgb-data->map [0.630, 0.340]
                                            [0.310, 0.595]
                                            [0.155, 0.070]
                                            wp/CIE-2-D65
                                            to-SMPTE240M from-SMPTE240M)
                 :ProPhoto-RGB (rgb-data->map [0.7347, 0.2653]
                                              [0.1596, 0.8404]
                                              [0.0366, 0.0001]
                                              wp/CIE-2-D50
                                              to-ROMM from-ROMM)
                 :Xtreme-RGB (rgb-data->map [1.0 0.0]
                                            [0.0 1.0]
                                            [0.0 0.0]
                                            wp/CIE-2-D50
                                            (->mgamma 0.45454545454545453)
                                            (->mgamma 2.2))}]
          (-> m
              (assoc :ITU-R-BT470-525 (:NTSC m))
              (assoc :ITU-R-BT470-625 (:PAL-SECAM m))
              (assoc :ROMM-RGB (:ProPhoto-RGB m)))))

(def ^:private rgb-space-keys #{:whitepoint :rgb-to-xyz :xyz-to-rgb :to :from})

(defn rgb-space
  "Returns an RGB space map.

  Parameters:

  - `space` - a key of [[rgbs]] (for example `:sRGB` or `:Adobe-RGB-1998`) or an RGB space map (see [[rgb-data->map]]), returned as is when it has the keys `:whitepoint`, `:rgb-to-xyz`, `:xyz-to-rgb`, `:to` and `:from` (non-nil).

  Throws `ex-info` with `{:space space}` for an unknown name or any other value, and with `{:missing-keys ks}` for a map without some of the required keys.

  See also [[rgb-spaces-list]]."
  [space]
  (cond
    (map? space) (let [missing (into (sorted-set) (remove #(some? (space %))) rgb-space-keys)]
                   (if (seq missing)
                     (throw (ex-info (str "RGB space map without keys: " missing) {:missing-keys missing}))
                     space))
    (contains? rgbs space) (rgbs space)
    :else (throw (ex-info (str "Unknown RGB space: " space) {:space space}))))

(def ^{:doc "Sorted names (keywords) of the predefined RGB spaces of [[rgbs]], aliases included."}
  rgb-spaces-list (sort (keys rgbs)))
