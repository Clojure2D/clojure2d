;; http://www.pegtop.net/delphi/articles/blendmodes/
;; http://www.simplefilter.de/en/basics/mixmods.html

(ns clojure2d.color.blend
  "Blend modes for color channels, colors, palettes and gradients.

  A blend mode combines two channel values: `a`, the base (backdrop, the lower layer, `cb` in W3C Compositing Level 1), and `b`, the blend (source, the upper layer, `cs`). Channel values are in `[0,255]`; formulas in the docstrings are written on the 0-1 scale (value / 255) unless stated otherwise.

  Per-channel modes, all `(f a b)` returning a double, are collected in [[blends]] (keyword to function) and [[blends-list]]:

  - clamped modes, results clamped to `[0,255]`; sources: W3C Compositing Level 1 ([[normal]], [[multiply]], [[screen]], [[overlay]], [[hardlight]], [[darken]], [[lighten]], [[dodge]], [[burn]], [[difference]], [[exclusion]]), Photoshop ([[add]], [[subtract]], [[linearburn]], [[linearlight]], [[vividlight]], [[pinlight]], [[hardmix]], [[divide]], [[softlight]]), W3C `soft-light` ([[w3c-softlight]]), GIMP ([[grainextract]], [[grainmerge]]) and pegtop.net ([[inversedodge]], [[inverseburn]], [[softdodge]], [[softburn]], [[reflect]], [[glow]], [[freeze]], [[heat]], [[stamp]], [[negation]], [[pegtoplight]], [[average]], [[phoenix]], [[interpolation]])
  - modulus modes, prefixed with `m` ([[madd]], [[mdodge]], ...): the author's artistic family, the unclamped formula of the clamped mode wrapped with mod 256 and capped at 255 (results in `[0,255]`; integer-valued results equal plain mod 256)
  - bitwise modes ([[or]], [[and]], [[xor]], [[nor]], [[nand]], [[xnor]]) on the channel values truncated to integers

  [[blend-fn]] looks a mode up by name (and throws for an unknown one); [[opacity]] mixes a mode's result with the base.

  Division by zero: a zero numerator gives 0, as for W3C `color-dodge` and `color-burn`; otherwise the clamped modes saturate and the modulus modes divide by `EPSILON` (1e-10) before wrapping.

  [[blend-colors]] blends two colors channel by channel (one mode for all channels or one mode per channel) and composites the result with the colors' alpha as in [W3C Compositing Level 1](https://www.w3.org/TR/compositing-1/#blending) ([[alpha-blending]]). [[blend-palettes]] and [[blend-gradients]] apply it to palettes and gradients.

  Colorspace blends [[hue]], [[saturation]], [[luminosity]] and [[color]] take one HSB channel from one color and the others from the second color; [[ch0]], [[ch1]] and [[ch2]] do the same for a channel of any colorspace. [[w3c-hue]], [[w3c-saturation]], [[w3c-color]] and [[w3c-luminosity]] are the W3C non-separable modes, which keep luminance (`0.3 r + 0.59 g + 0.11 b`) instead of HSB brightness; [[darker-color]] and [[lighter-color]] pick the whole color with the lower or higher luminance. These whole-color modes are collected in [[color-blends]]."
  (:refer-clojure :exclude [or and])
  (:require [fastmath.core :as m]
            [clojure2d.color :as c])
  (:import [fastmath.vector Vec4]))

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

(defmacro ^:private clamp255 [v] `(m/constrain ~v 0.0 255.0))
(defmacro ^:private wrap256
  "Wraps `v` with mod 256 and caps the result at 255."
  [v] `(m/min (m/mod ~v 256.0) 255.0))

(def ^{:const true :private true :tag 'double} r255 (m// 255.0))
(def ^{:const true :private true :tag 'double} r255-2 (m// (m/* 255.0 255.0)))
(def ^{:const true :private true :tag 'double} rsqrt255 (m// (m/sqrt 255)))

(defn normal
  "Normal mode: the blend value.

  Formula: `b`. W3C Compositing Level 1 `normal`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[blends]]."
  ^double [^double _ ^double b] b)

(defn add
  "Add mode (Photoshop linear dodge).

  Formula: `a + b`, clamped. Pillow `ImageChops.add`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[madd]], [[subtract]]."
  ^double [^double a ^double b]
  (clamp255 (m/+ a b)))

(defn madd
  "Modulus add mode.

  Formula on the 0-255 scale: `a + b`, mod 256, capped at 255. Pillow `ImageChops.add_modulo` for integer values. Part of the author's modulus family.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the wrapped value in `[0,255]`. See also [[add]], [[msubtract]]."
  ^double [^double a ^double b]
  (wrap256 (m/+ a b)))

(defn subtract
  "Subtract mode.

  Formula: `a - b`, clamped. Pillow `ImageChops.subtract`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[msubtract]], [[add]]."
  ^double [^double a ^double b]
  (clamp255 (m/- a b)))

(defn msubtract
  "Modulus subtract mode.

  Formula on the 0-255 scale: `a - b`, mod 256, capped at 255. Pillow `ImageChops.subtract_modulo` for integer values. Part of the author's modulus family.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the wrapped value in `[0,255]`. See also [[subtract]], [[madd]]."
  ^double [^double a ^double b]
  (wrap256 (m/- a b)))

(defn linearburn
  "Linear burn mode (Photoshop).

  Formula: `a + b - 1`, clamped.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[mlinearburn]], [[burn]]."
  ^double [^double a ^double b]
  (clamp255 (m/- (m/+ a b) 255.0)))

(defn mlinearburn
  "Modulus linear burn mode.

  Formula on the 0-255 scale: `a + b - 255`, mod 256, capped at 255. Part of the author's modulus family.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the wrapped value in `[0,255]`. See also [[linearburn]]."
  ^double [^double a ^double b]
  (wrap256 (m/- (m/+ a b) 255.0)))

(defn darken
  "Darken mode: the darker value.

  Formula: `min(a, b)`. W3C `darken`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[lighten]]."
  ^double [^double a ^double b] 
  (m/min a b))

(defn lighten
  "Lighten mode: the lighter value.

  Formula: `max(a, b)`. W3C `lighten`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[darken]]."
  ^double [^double a ^double b]
  (m/max a b))

(defn multiply
  "Multiply mode: darkens.

  Formula: `a * b`. W3C `multiply`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[screen]]."
  ^double [^double a ^double b]
  (m/* a b r255))

(defn screen
  "Screen mode: lightens.

  Formula: `a + b - a * b`. W3C `screen`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[multiply]]."
  ^double [^double a ^double b]
  (let [ra (m/- 255.0 a)
        rb (m/- 255.0 b)]
    (m/- 255.0 (m/* r255 rb ra ))))

(defn dodge
  "Color dodge mode: brightens the base by the blend.

  Formula: `a / (1 - b)`, clamped; 0 when `a = 0`, 1 when `b = 1` and `a > 0`. W3C `color-dodge`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[mdodge]], [[inversedodge]], [[softdodge]], [[burn]]."
  ^double [^double a ^double b]
  (clamp255 (m// (m/* 255.0 a) (m/max m/EPSILON (m/- 255.0 b)))))

(defn mdodge
  "Modulus color dodge mode.

  Formula on the 0-255 scale: `255 * a / (255 - b)`, mod 256, capped at 255. The denominator is guarded by `EPSILON`, so a zero denominator gives a value about 1e10 times the numerator before wrapping. Part of the author's modulus family.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the wrapped value in `[0,255]`. See also [[dodge]]."
  ^double [^double a ^double b]
  (wrap256 (m// (m/* 255.0 a) (m/max m/EPSILON (m/- 255.0 b)))))

(defn inversedodge
  "Inverse color dodge mode (pegtop.net): color dodge with the layers swapped.

  Formula: `b / (1 - a)`, clamped; 0 when `b = 0`, 1 when `a = 1` and `b > 0`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[minversedodge]], [[dodge]]."
  ^double [^double a ^double b]
  (clamp255 (m// (m/* 255.0 b) (m/max m/EPSILON (m/- 255.0 a)))))

(defn minversedodge
  "Modulus inverse color dodge mode.

  Formula on the 0-255 scale: `255 * b / (255 - a)`, mod 256, capped at 255. The denominator is guarded by `EPSILON`, so a zero denominator gives a value about 1e10 times the numerator before wrapping. Part of the author's modulus family.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the wrapped value in `[0,255]`. See also [[inversedodge]]."
  ^double [^double a ^double b]
  (wrap256 (m// (m/* 255.0 b) (m/max m/EPSILON (m/- 255.0 a)))))

(defn softdodge
  "Soft dodge mode (pegtop.net): a softer color dodge.

  Formula: `a / (2 * (1 - b))` when `a + b <= 1`, else `1 - (1 - b) / (2 * a)`, clamped; 0 when `a = 0`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[msoftdodge]], [[dodge]], [[softburn]]."
  ^double [^double a ^double b]
  (if (m/<= (m/+ a b) 255.0)
    (clamp255 (m// (m/* 127.5 a) (m/max m/EPSILON (m/- 255.0 b))))
    (clamp255 (m/- 255.0 (m/* 127.5 (m// (m/- 255.0 b) (m/max m/EPSILON a)))))))

(defn msoftdodge
  "Modulus soft dodge mode.

  Formula on the 0-255 scale: `127.5 * a / (255 - b)` when `a + b <= 255`, else `255 - 127.5 * (255 - b) / a`, mod 256, capped at 255. The denominator is guarded by `EPSILON`, so a zero denominator gives a value about 1e10 times the numerator before wrapping. Part of the author's modulus family.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the wrapped value in `[0,255]`. See also [[softdodge]]."
  ^double [^double a ^double b]
  (if (m/<= (m/+ a b) 255.0)
    (wrap256 (m// (m/* 127.5 a) (m/max m/EPSILON (m/- 255.0 b))))
    (wrap256 (m/- 255.0 (m/* 127.5 (m// (m/- 255.0 b) (m/max m/EPSILON a)))))))

(defn burn
  "Color burn mode: darkens the base by the blend.

  Formula: `1 - (1 - a) / b`, clamped; 1 when `a = 1`, 0 when `b = 0` and `a < 1`. W3C `color-burn`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[mburn]], [[inverseburn]], [[softburn]], [[dodge]]."
  ^double [^double a ^double b]
  (clamp255 (m/* 255.0 (m/- 1.0 (m// (m/- 255.0 a) (m/max m/EPSILON b))))))

(defn mburn
  "Modulus color burn mode.

  Formula on the 0-255 scale: `255 * (1 - (255 - a) / b)`, mod 256, capped at 255. The denominator is guarded by `EPSILON`, so a zero denominator gives a value about 1e10 times the numerator before wrapping. Part of the author's modulus family.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the wrapped value in `[0,255]`. See also [[burn]]."
  ^double [^double a ^double b]
  (wrap256 (m/* 255.0 (m/- 1.0 (m// (m/- 255.0 a) (m/max m/EPSILON b))))))

(defn inverseburn
  "Inverse color burn mode (pegtop.net): color burn with the layers swapped.

  Formula: `1 - (1 - b) / a`, clamped; 1 when `b = 1`, 0 when `a = 0` and `b < 1`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[minverseburn]], [[burn]]."
  ^double [^double a ^double b]
  (clamp255 (m/* 255.0 (m/- 1.0 (m// (m/- 255.0 b) (m/max m/EPSILON a))))))

(defn minverseburn
  "Modulus inverse color burn mode.

  Formula on the 0-255 scale: `255 * (1 - (255 - b) / a)`, mod 256, capped at 255. The denominator is guarded by `EPSILON`, so a zero denominator gives a value about 1e10 times the numerator before wrapping. Part of the author's modulus family.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the wrapped value in `[0,255]`. See also [[inverseburn]]."
  ^double [^double a ^double b]
  (wrap256 (m/* 255.0 (m/- 1.0 (m// (m/- 255.0 b) (m/max m/EPSILON a))))))

(defn softburn
  "Soft burn mode (pegtop.net): a softer color burn.

  Formula: `b / (2 * (1 - a))` when `a + b <= 1`, else `1 - (1 - a) / (2 * b)`, clamped; continuous at `a + b = 1` (value 0.5), 0 when `b = 0`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[msoftburn]], [[burn]], [[softdodge]]."
  ^double [^double a ^double b]
  (if (m/<= (m/+ a b) 255.0)
    (clamp255 (m/* 127.5 (m// b (m/max m/EPSILON (m/- 255.0 a)))))
    (clamp255 (m/- 255.0 (m/* 127.5 (m// (m/- 255.0 a) (m/max m/EPSILON b)))))))

(defn msoftburn
  "Modulus soft burn mode.

  Formula on the 0-255 scale: `127.5 * b / (255 - a)` when `a + b <= 255`, else `255 - 127.5 * (255 - a) / b`, mod 256, capped at 255. The denominator is guarded by `EPSILON`, so a zero denominator gives a value about 1e10 times the numerator before wrapping. Part of the author's modulus family.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the wrapped value in `[0,255]`. See also [[softburn]]."
  ^double [^double a ^double b]
  (if (m/<= (m/+ a b) 255.0)
    (wrap256 (m/* 127.5 (m// b (m/max m/EPSILON (m/- 255.0 a)))))
    (wrap256 (m/- 255.0 (m/* 127.5 (m// (m/- 255.0 a) (m/max m/EPSILON b)))))))

(defn hardmix
  "Hard mix mode (Photoshop): posterizes to 0 or 255.

  Formula: 1 when `a + b >= 1`, else 0.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[vividlight]]."
  ^double [^double a ^double b]
  (let [t (m/- 255.0 b)]
    (if (m/< a t) 0.0 255.0)))

(defn linearlight
  "Linear light mode (Photoshop): linear burn or linear dodge depending on the blend.

  Formula: `a + 2 * b - 1`, clamped. Same values as [[stamp]].

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[mlinearlight]], [[linearburn]], [[add]]."
  ^double [^double a ^double b]
  (clamp255 (m/- (m/+ a b b) 255.0)))

(defn mlinearlight
  "Modulus linear light mode.

  Formula on the 0-255 scale: `a + 2 * b - 255`, mod 256, capped at 255. Same values as [[mstamp]]. Part of the author's modulus family.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the wrapped value in `[0,255]`. See also [[linearlight]]."
  ^double [^double a ^double b]
  (wrap256 (m/- (m/+ a b b) 255.0)))

(defn stamp
  "Stamp mode (pegtop.net).

  Formula: `a + 2 * b - 1`, clamped.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[mstamp]], [[linearburn]]."
  ^double [^double a ^double b]
  (clamp255 (m/- (m/+ b b a) 255.0)))

(defn mstamp
  "Modulus stamp mode.

  Formula on the 0-255 scale: `a + 2 * b - 255`, mod 256, capped at 255. Part of the author's modulus family.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the wrapped value in `[0,255]`. See also [[stamp]]."
  ^double [^double a ^double b]
  (wrap256 (m/- (m/+ b b a) 255.0)))

(defn difference
  "Difference mode.

  Formula: `|a - b|`. W3C `difference`, Pillow `ImageChops.difference`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[exclusion]], [[negation]]."
  ^double [^double a ^double b]
  (m/abs (m/- a b)))

(defn divide
  "Divide mode (Photoshop, GIMP): the base divided by the blend.

  Formula: `a / b`, clamped; 0 when `a = 0`, 1 when `b = 0` and `a > 0`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[mdivide]]."
  ^double [^double a ^double b]
  (clamp255 (m/* 255.0 (m// a (m/max m/EPSILON b)))))

(defn mdivide
  "Modulus divide mode.

  Formula on the 0-255 scale: `255 * a / b`, mod 256, capped at 255. The denominator is guarded by `EPSILON`, so a zero denominator gives a value about 1e10 times the numerator before wrapping. Part of the author's modulus family.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the wrapped value in `[0,255]`. See also [[divide]]."
  ^double [^double a ^double b]
  (wrap256 (m/* 255.0 (m// a (m/max m/EPSILON b)))))

(defn or
  "Bitwise `or` of the channel values.

  Both values are truncated to integers; the result keeps the low 8 bits. Part of the author's artistic modes.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns an integer-valued double in `[0,255]`. See also [[nor]], [[and]], [[xor]]."
  ^double [^double a ^double b]
  (double (m/bit-and 0xff (m/bit-or (unchecked-long a) (unchecked-long b)))))

(defn and
  "Bitwise `and` of the channel values.

  Both values are truncated to integers; the result keeps the low 8 bits. Part of the author's artistic modes.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns an integer-valued double in `[0,255]`. See also [[nand]], [[or]], [[xor]]."
  ^double [^double a ^double b]
  (double (m/bit-and 0xff (m/bit-and (unchecked-long a) (unchecked-long b)))))

(defn xor
  "Bitwise `xor` of the channel values.

  Both values are truncated to integers; the result keeps the low 8 bits. Part of the author's artistic modes.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns an integer-valued double in `[0,255]`. See also [[xnor]], [[or]], [[and]]."
  ^double [^double a ^double b]
  (double (m/bit-and 0xff (m/bit-xor (unchecked-long a) (unchecked-long b)))))

(defn nor
  "Bitwise `nor` of the channel values.

  Both values are truncated to integers; the result keeps the low 8 bits. Part of the author's artistic modes.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns an integer-valued double in `[0,255]`. See also [[or]]."
  ^double [^double a ^double b]
  (double (m/bit-and 0xff (m/bit-nor (unchecked-long a) (unchecked-long b)))))

(defn nand
  "Bitwise `nand` of the channel values.

  Both values are truncated to integers; the result keeps the low 8 bits. Part of the author's artistic modes.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns an integer-valued double in `[0,255]`. See also [[and]]."
  ^double [^double a ^double b]
  (double (m/bit-and 0xff (m/bit-nand (unchecked-long a) (unchecked-long b)))))

(defn xnor
  "Bitwise `xnor` of the channel values.

  Both values are truncated to integers; the result keeps the low 8 bits. Part of the author's artistic modes.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns an integer-valued double in `[0,255]`. See also [[xor]]."
  ^double [^double a ^double b]
  (double (m/bit-and 0xff (m/bit-xnor (unchecked-long a) (unchecked-long b)))))

(defn exclusion
  "Exclusion mode: a lower-contrast difference.

  Formula: `a + b - 2 * a * b`. W3C `exclusion`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[difference]]."
  ^double [^double a ^double b]
  (let [ab (m/* a b r255)]
    (m/- (m/+ a b) (m/+ ab ab))))

(defn pinlight
  "Pin light mode (Photoshop): darken or lighten depending on the blend.

  Formula: the base clamped into `[2 * b - 1, 2 * b]`, i.e. `min(a, 2 * b)` when `b < 0.5`, else `max(a, 2 * b - 1)`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[darken]], [[lighten]]."
  ^double [^double a ^double b]
  (let [b2 (m/+ b b)]
    (m/constrain a (m/- b2 255.0) b2)))

(defn average
  "Average mode.

  Formula: `(a + b) / 2`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[normal]]."
  ^double [^double a ^double b]
  (m// (m/+ a b) 2.0))

(defn negation
  "Negation mode (pegtop.net).

  Formula: `1 - |1 - a - b|`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[difference]]."
  ^double [^double a ^double b]
  (m/- 255.0 (m/abs (m/- 255.0 a b))))

(defn reflect
  "Reflect mode (pegtop.net): strong brightening of light blend values.

  Formula: `a * a / (1 - b)`, clamped; 0 when `a = 0`, 1 when `b = 1` and `a > 0`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[mreflect]], [[glow]]."
  ^double [^double a ^double b]
  (clamp255 (m// (m/* a a) (m/max m/EPSILON (m/- 255.0 b)))))

(defn mreflect
  "Modulus reflect mode.

  Formula on the 0-255 scale: `a * a / (255 - b)`, mod 256, capped at 255. The denominator is guarded by `EPSILON`, so a zero denominator gives a value about 1e10 times the numerator before wrapping. Part of the author's modulus family.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the wrapped value in `[0,255]`. See also [[reflect]]."
  ^double [^double a ^double b]
  (wrap256 (m// (m/* a a) (m/max m/EPSILON (m/- 255.0 b)))))

(defn glow
  "Glow mode (pegtop.net): reflect with the layers swapped.

  Formula: `b * b / (1 - a)`, clamped; 0 when `b = 0`, 1 when `a = 1` and `b > 0`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[mglow]], [[reflect]]."
  ^double [^double a ^double b]
  (clamp255 (m// (m/* b b) (m/max m/EPSILON (m/- 255.0 a)))))

(defn mglow
  "Modulus glow mode.

  Formula on the 0-255 scale: `b * b / (255 - a)`, mod 256, capped at 255. The denominator is guarded by `EPSILON`, so a zero denominator gives a value about 1e10 times the numerator before wrapping. Part of the author's modulus family.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the wrapped value in `[0,255]`. See also [[glow]]."
  ^double [^double a ^double b]
  (wrap256 (m// (m/* b b) (m/max m/EPSILON (m/- 255.0 a)))))

(defn freeze
  "Freeze mode (pegtop.net): strong darkening of dark blend values.

  Formula: `1 - (1 - a) * (1 - a) / b`, clamped; 1 when `a = 1`, 0 when `b = 0` and `a < 1`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[mfreeze]], [[heat]]."
  ^double [^double a ^double b]
  (clamp255 (m/- 255.0 (m// (m/sq (m/- 255.0 a)) (m/max m/EPSILON b)))))

(defn mfreeze
  "Modulus freeze mode.

  Formula on the 0-255 scale: `255 - (255 - a) * (255 - a) / b`, mod 256, capped at 255. The denominator is guarded by `EPSILON`, so a zero denominator gives a value about 1e10 times the numerator before wrapping. Part of the author's modulus family.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the wrapped value in `[0,255]`. See also [[freeze]]."
  ^double [^double a ^double b]
  (wrap256 (m/- 255.0 (m// (m/sq (m/- 255.0 a)) (m/max m/EPSILON b)))))

(defn heat
  "Heat mode (pegtop.net): freeze with the layers swapped.

  Formula: `1 - (1 - b) * (1 - b) / a`, clamped; 1 when `b = 1`, 0 when `a = 0` and `b < 1`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[mheat]], [[freeze]]."
  ^double [^double a ^double b]
  (clamp255 (m/- 255.0 (m// (m/sq (m/- 255.0 b)) (m/max m/EPSILON a)))))

(defn mheat
  "Modulus heat mode.

  Formula on the 0-255 scale: `255 - (255 - b) * (255 - b) / a`, mod 256, capped at 255. The denominator is guarded by `EPSILON`, so a zero denominator gives a value about 1e10 times the numerator before wrapping. Part of the author's modulus family.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the wrapped value in `[0,255]`. See also [[heat]]."
  ^double [^double a ^double b]
  (wrap256 (m/- 255.0 (m// (m/sq (m/- 255.0 b)) (m/max m/EPSILON a)))))

(defn overlay
  "Overlay mode: hard light with the layers swapped.

  Formula: `2 * a * b` when `a < 0.5`, else `1 - 2 * (1 - a) * (1 - b)`. W3C `overlay`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[hardlight]], [[softlight]]."
  ^double [^double a ^double b]
  (if (m/< a 127.5)
    (m/* 2.0 (m/* a b r255))
    (m/- 255.0 (m/* 2.0 (m/* r255 (m/- 255.0 a) (m/- 255.0 b))))))

(defn hardlight
  "Hard light mode: multiply or screen depending on the blend.

  Formula: `2 * a * b` when `b < 0.5`, else `1 - 2 * (1 - a) * (1 - b)`. W3C `hard-light`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[overlay]], [[softlight]]."
  ^double [^double a ^double b]
  (if (m/< b 127.5)
    (m/* 2.0 (m/* a b r255))
    (m/- 255.0 (m/* 2.0 (m/* r255 (m/- 255.0 a) (m/- 255.0 b))))))

(defn softlight
  "Soft light mode, Photoshop variant (formula from pegtop.net).

  Formula: `a - (1 - 2 * b) * a * (1 - a)` when `b < 0.5`, else `a + (2 * b - 1) * (sqrt(a) - a)`. Not the W3C `soft-light`, which differs by up to 16.9 on the 0-255 scale.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[pegtoplight]], [[hardlight]]."
  ^double [^double a ^double b]
  (if (m/< b 127.5)
    (m/+ a (m/* (m/+ b b -255.0) (m/* a (m/- 255.0 a) r255-2)))
    (m/+ a (m/* (m/+ b b -255.0) (m/- (m/* (m/sqrt a) rsqrt255)
                                      (m/* a r255))))))

(defn pegtoplight
  "Soft light mode, pegtop.net variant: continuous, without branches.

  Formula: `(1 - 2 * b) * a * a + 2 * a * b`. Pillow `ImageChops.soft_light`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[softlight]]."
  ^double [^double a ^double b]
  (m/+ (m/* 2.0  a b r255-2 (m/- 255.0 a))
       (m/* a a r255)))

(defn vividlight
  "Vivid light mode (Photoshop): color burn or color dodge depending on the blend.

  Formula: `1 - (1 - a) / (2 * b)` when `b < 0.5`, else `a / (2 * (1 - b))`, clamped.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[mvividlight]], [[burn]], [[dodge]]."
  ^double [^double a ^double b]
  (if (m/< b 127.5)
    (clamp255 (m/- 255.0 (m// (m/* 255.0 (m/- 255.0 a)) (m/max m/EPSILON (m/+ b b)))))
    (clamp255 (m// (m/* 255.0 a) (m/max m/EPSILON (m/* 2.0 (m/- 255.0 b)))))))

(defn mvividlight
  "Modulus vivid light mode.

  Formula on the 0-255 scale: `255 - 255 * (255 - a) / (2 * b)` when `b < 127.5`, else `255 * a / (2 * (255 - b))`, mod 256, capped at 255. The denominator is guarded by `EPSILON`, so a zero denominator gives a value about 1e10 times the numerator before wrapping. Part of the author's modulus family.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the wrapped value in `[0,255]`. See also [[vividlight]]."
  ^double [^double a ^double b]
  (if (m/< b 127.5)
    (wrap256 (m/- 255.0 (m// (m/* 255.0 (m/- 255.0 a)) (m/max m/EPSILON (m/+ b b)))))
    (wrap256 (m// (m/* 255.0 a) (m/max m/EPSILON (m/* 2.0 (m/- 255.0 b)))))))

(defn w3c-softlight
  "Soft light mode, W3C variant.

  Formula: `a - (1 - 2 * b) * a * (1 - a)` when `b <= 0.5`, else `a + (2 * b - 1) * (D(a) - a)` with `D(a) = ((16 * a - 12) * a + 4) * a` for `a <= 0.25` and `sqrt(a)` otherwise. W3C `soft-light`, as used by CSS.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[softlight]], [[pegtoplight]]."
  ^double [^double a ^double b]
  (let [a1 (m/* a r255)
        b1 (m/* b r255)]
    (m/* 255.0 (if (m/<= b1 0.5)
                 (m/- a1 (m/* (m/- 1.0 (m/* 2.0 b1)) a1 (m/- 1.0 a1)))
                 (let [d (if (m/<= a1 0.25)
                           (m/* (m/+ (m/* (m/- (m/* 16.0 a1) 12.0) a1) 4.0) a1)
                           (m/sqrt a1))]
                   (m/+ a1 (m/* (m/- (m/* 2.0 b1) 1.0) (m/- d a1))))))))

(defn grainextract
  "Grain extract mode (GIMP).

  Formula: `a - b + 0.5`, clamped.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[grainmerge]], [[subtract]]."
  ^double [^double a ^double b]
  (clamp255 (m/+ (m/- a b) 127.5)))

(defn grainmerge
  "Grain merge mode (GIMP).

  Formula: `a + b - 0.5`, clamped.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[grainextract]], [[add]]."
  ^double [^double a ^double b]
  (clamp255 (m/- (m/+ a b) 127.5)))

(defn phoenix
  "Phoenix mode (pegtop.net).

  Formula: `min(a, b) - max(a, b) + 1`, i.e. `1 - |a - b|`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[difference]]."
  ^double [^double a ^double b]
  (m/+ (m/- (m/min a b) (m/max a b)) 255.0))

(defn interpolation
  "Interpolation mode (pegtop.net): a smooth, cosine-shaped average.

  Formula: `0.5 - 0.25 * cos(pi * a) - 0.25 * cos(pi * b)`.

  - `a` base (backdrop) channel value, `[0,255]`
  - `b` blend (source) channel value, `[0,255]`

  Returns the blended value in `[0,255]`. See also [[average]]."
  ^double [^double a ^double b]
  (m/* 63.75 (m/- 2.0 (m/cos (m/* m/PI a r255)) (m/cos (m/* m/PI b r255)))))

(def ^{:doc "Per-channel modes by name: keyword to `(f a b)` (see the namespace docstring). Use with [[blend-colors]], `clojure2d.pixels` and `clojure2d.color.cssgram`."}
  blends {:normal normal
           :add add
           :madd madd
           :subtract subtract
           :msubtract msubtract
           :linearburn linearburn
           :mlinearburn mlinearburn
           :darken darken
           :lighten lighten
           :multiply multiply
           :screen screen
           :dodge dodge
           :mdodge mdodge
           :inversedodge inversedodge
           :minversedodge minversedodge
           :softdodge softdodge
           :msoftdodge msoftdodge
           :burn burn
           :mburn mburn
           :inverseburn inverseburn
           :minverseburn minverseburn
           :softburn softburn
           :msoftburn msoftburn
           :hardmix hardmix
           :linearlight linearlight
           :mlinearlight mlinearlight
           :stamp stamp
           :mstamp mstamp
           :difference difference
           :divide divide
           :mdivide mdivide
           :or or
           :and and
           :xor xor
           :nor nor
           :nand nand
           :xnor xnor
           :exclusion exclusion
           :pinlight pinlight
           :average average
           :negation negation
           :reflect reflect
           :mreflect mreflect
           :glow glow
           :mglow mglow
           :freeze freeze
           :mfreeze mfreeze
           :heat heat
           :mheat mheat
           :overlay overlay
           :hardlight hardlight
           :softlight softlight
           :pegtoplight pegtoplight
           :vividlight vividlight
           :mvividlight mvividlight
           :w3c-softlight w3c-softlight
           :grainextract grainextract
           :grainmerge grainmerge
           :phoenix phoenix
           :interpolation interpolation})

(def ^{:doc "Sorted list of the per-channel mode names (keywords of [[blends]])."} blends-list (sort (keys blends)))

(defn blend-fn
  "Per-channel mode for a name or a function.

  - `mode` a keyword of [[blends]] or a function `(f a b)`

  Returns the function. Throws `ex-info` with `{:blend mode}` for an unknown keyword, `nil` or any other value.

  See also [[blends]], [[opacity]]."
  [mode]
  (cond
    (fn? mode) mode
    :else (clojure.core/or (get blends mode)
                           (throw (ex-info (str "Unknown blend mode: " mode) {:blend mode})))))

(defn opacity
  "Per-channel mode applied with an opacity: mixes the base with the mode's result.

  Formula: `(1 - t) * a + t * f(a, b)`, exactly `a` for `t = 0` and `f(a, b)` for `t = 1`.

  - `mode` a keyword of [[blends]] or a function `(f a b)` (see [[blend-fn]])
  - `t` opacity, `0.0` returns the base, `1.0` the mode's result

  Returns a per-channel mode `(f a b)`. See also [[blend-colors]]."
  [mode ^double t]
  (let [f (blend-fn mode)]
    (fn ^double [^double a ^double b]
      (m/+ (m/* (m/- 1.0 t) a) (m/* t (double (f a b)))))))

(defn alpha-blending
  "Composites a blended color over the backdrop with the colors' alpha (W3C Compositing Level 1 source-over with blending).

  With `ab`, `as` the alphas of `cb` and `cs` on the 0-1 scale, each channel of the result is `(as * (1 - ab) * cs + as * ab * blend + (1 - as) * ab * cb) / ao`, where `ao = as + ab * (1 - as)`. Colors are not premultiplied.

  - `cb` backdrop color, `Vec4` with channels and alpha in `[0,255]`
  - `cs` source color, `Vec4`
  - `blend` the blended color B(cb, cs), `Vec4`; its alpha is ignored

  Returns a `Vec4` with the composited channels and alpha `255 * ao`. When both alphas are 0, returns the channels of `blend` with alpha 0.

  See also [[blend-colors]]."
  ^Vec4 [^Vec4 cb ^Vec4 cs ^Vec4 blend]
  (let [ab (m/* r255 (.w cb))
        as (m/* r255 (.w cs))
        a1 (m/* as (m/- 1.0 ab))
        a2 (m/* as ab)
        a3 (m/* (m/- 1.0 as) ab)
        a0 (m/+ as a3)]
    (if (m/zero? a0)
      (Vec4. (.x blend) (.y blend) (.z blend) 0.0)
      (Vec4. (m// (m/+ (m/* a1 (.x cs)) (m/* a2 (.x blend)) (m/* a3 (.x cb))) a0)
             (m// (m/+ (m/* a1 (.y cs)) (m/* a2 (.y blend)) (m/* a3 (.y cb))) a0)
             (m// (m/+ (m/* a1 (.z cs)) (m/* a2 (.z blend)) (m/* a3 (.z cb))) a0)
             (m/* 255.0 a0)))))

(defn blend-colors
  "Blends two colors channel by channel and composites the result with their alpha.

  - `mode` per-channel mode used for all three channels: a keyword of [[blends]] or a function `(f a b)`, e.g. `:multiply` or [[multiply]]
  - `mode1`, `mode2`, `mode3` modes for the first, second and third channel
  - `cb` backdrop (base) color, anything accepted by `clojure2d.color/to-color`
  - `cs` source (blend) color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. When both colors are opaque, the result is the blended channels with alpha 255; otherwise they are composited with [[alpha-blending]]. Channels are blended as stored, so colors in another colorspace are blended channel-wise as well. Throws `ex-info` with `{:blend mode}` for an unknown mode (see [[blend-fn]]).

  See also [[blend-palettes]], [[blend-gradients]]."
  (^Vec4 [mode cb cs] (blend-colors mode mode mode cb cs))
  (^Vec4 [mode1 mode2 mode3 cb cs]
   (let [f1 (blend-fn mode1)
         f2 (blend-fn mode2)
         f3 (blend-fn mode3)
         cb (c/to-color cb)
         cs (c/to-color cs)
         blend (Vec4. (f1 (.x cb) (.x cs))
                      (f2 (.y cb) (.y cs))
                      (f3 (.z cb) (.z cs))
                      255.0)]
     (if (clojure.core/and (m/>= (.w cb) 255.0)
                           (m/>= (.w cs) 255.0))
       blend
       (alpha-blending cb cs blend)))))

(defn blend-palettes
  "Blends two palettes color by color with [[blend-colors]].

  - `mode` or `mode1`, `mode2`, `mode3` per-channel modes, as in [[blend-colors]]
  - `pal1` backdrop palette, anything accepted by `clojure2d.color/palette`
  - `pal2` source palette, anything accepted by `clojure2d.color/palette`

  Returns a vector of `Vec4` colors, as long as the shorter palette.

  See also [[blend-gradients]]."
  ([mode pal1 pal2] (blend-palettes mode mode mode pal1 pal2))
  ([mode1 mode2 mode3 pal1 pal2]
   (mapv (partial blend-colors (blend-fn mode1) (blend-fn mode2) (blend-fn mode3)) (c/palette pal1) (c/palette pal2))))

(defn blend-gradients
  "Blends two gradients with [[blend-colors]].

  - `mode` or `mode1`, `mode2`, `mode3` per-channel modes, as in [[blend-colors]]
  - `g1` backdrop gradient, a function of `t` in `[0,1]` returning a color (e.g. from `clojure2d.color/gradient`)
  - `g2` source gradient, a function of `t`

  Returns a gradient: a function of `t` returning the blend of `(g1 t)` and `(g2 t)`.

  See also [[blend-palettes]]."
  ([mode g1 g2] (blend-gradients mode mode mode g1 g2))
  ([mode1 mode2 mode3 g1 g2]
   (let [f1 (blend-fn mode1)
         f2 (blend-fn mode2)
         f3 (blend-fn mode3)]
     (fn [^double t] (blend-colors f1 f2 f3 (g1 t) (g2 t))))))

;;

(defn- cs-op
  "Channel `ch` of `cs` with the other channels of `cb` in `colorspace`, composited with [[alpha-blending]]."
  [colorspace cb cs ch]
  (let [[to from] (clojure.core/or (c/colorspaces colorspace)
                                   (throw (ex-info (str "Unknown colorspace: " colorspace) {:colorspace colorspace})))
        b (to cb)
        s (to cs)]
    (alpha-blending (c/to-color cb) (c/to-color cs) (from (c/set-channel b ch (c/get-channel s ch))))))

(defn hue
  "HSB hue blend: the hue of the source with the saturation and brightness of the backdrop.

  Unlike the W3C `hue` mode, it keeps HSB brightness (the largest channel), not luminance. The result is composited with the colors' alpha by [[alpha-blending]].

  - `cb` backdrop color, anything accepted by `clojure2d.color/to-color`
  - `cs` source color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. See also [[saturation]], [[luminosity]], [[color]], [[ch0]]."
  [cb cs] (cs-op :HSB cb cs 0))

(defn saturation
  "HSB saturation blend: the saturation of the source with the hue and brightness of the backdrop.

  Unlike the W3C `saturation` mode, it keeps HSB brightness (the largest channel), not luminance. The result is composited with the colors' alpha by [[alpha-blending]].

  - `cb` backdrop color, anything accepted by `clojure2d.color/to-color`
  - `cs` source color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. See also [[hue]], [[luminosity]], [[color]], [[ch1]]."
  [cb cs] (cs-op :HSB cb cs 1))

(defn luminosity
  "HSB brightness blend: the brightness of the source with the hue and saturation of the backdrop.

  Unlike the W3C `luminosity` mode, it uses HSB brightness (the largest channel), not luminance. The result is composited with the colors' alpha by [[alpha-blending]].

  - `cb` backdrop color, anything accepted by `clojure2d.color/to-color`
  - `cs` source color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. See also [[color]], [[hue]], [[saturation]], [[ch2]]."
  [cb cs] (cs-op :HSB cb cs 2))

(defn color
  "HSB color blend: the hue and saturation of the source with the brightness of the backdrop.

  The inverse of [[luminosity]]. Unlike the W3C `color` mode, it uses HSB brightness (the largest channel), not luminance. The result is composited with the colors' alpha by [[alpha-blending]].

  - `cb` backdrop color, anything accepted by `clojure2d.color/to-color`
  - `cs` source color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. See also [[luminosity]], [[hue]], [[saturation]]."
  [cb cs] (cs-op :HSB cs cb 2))

(defn ch0
  "Channel blend: channel 0 of the source with channels 1 and 2 of the backdrop, in a colorspace.

  - `colorspace` keyword of `clojure2d.color/colorspaces` (raw ranges), default `:RGB`
  - `cb` backdrop color, anything accepted by `clojure2d.color/to-color`
  - `cs` source color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4` composited with the colors' alpha by [[alpha-blending]]. Throws `ex-info` with `{:colorspace colorspace}` for an unknown colorspace. See also [[ch1]], [[ch2]], [[hue]]."
  ([colorspace cb cs] (cs-op colorspace cb cs 0)) ([cb cs] (ch0 :RGB cb cs)))

(defn ch1
  "Channel blend: channel 1 of the source with channels 0 and 2 of the backdrop, in a colorspace.

  - `colorspace` keyword of `clojure2d.color/colorspaces` (raw ranges), default `:RGB`
  - `cb` backdrop color, anything accepted by `clojure2d.color/to-color`
  - `cs` source color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4` composited with the colors' alpha by [[alpha-blending]]. Throws `ex-info` with `{:colorspace colorspace}` for an unknown colorspace. See also [[ch0]], [[ch2]], [[saturation]]."
  ([colorspace cb cs] (cs-op colorspace cb cs 1)) ([cb cs] (ch1 :RGB cb cs)))

(defn ch2
  "Channel blend: channel 2 of the source with channels 0 and 1 of the backdrop, in a colorspace.

  - `colorspace` keyword of `clojure2d.color/colorspaces` (raw ranges), default `:RGB`
  - `cb` backdrop color, anything accepted by `clojure2d.color/to-color`
  - `cs` source color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4` composited with the colors' alpha by [[alpha-blending]]. Throws `ex-info` with `{:colorspace colorspace}` for an unknown colorspace. See also [[ch0]], [[ch1]], [[luminosity]]."
  ([colorspace cb cs] (cs-op colorspace cb cs 2)) ([cb cs] (ch2 :RGB cb cs)))

;; W3C non-separable modes and whole-color modes

(defn- lum
  "W3C `Lum`: `0.3 r + 0.59 g + 0.11 b`."
  ^double [^Vec4 c]
  (m/+ (m/* 0.3 (.x c)) (m/* 0.59 (.y c)) (m/* 0.11 (.z c))))

(defn- scale-around
  "Channels moved towards `l` by factor `f`: `l + (c - l) * f`."
  ^Vec4 [^Vec4 c ^double l ^double f]
  (Vec4. (m/+ l (m/* (m/- (.x c) l) f))
         (m/+ l (m/* (m/- (.y c) l) f))
         (m/+ l (m/* (m/- (.z c) l) f))
         (.w c)))

(defn- clip-color
  "W3C `ClipColor` on the 0-255 scale: brings the channels into `[0,255]` keeping `Lum`."
  ^Vec4 [^Vec4 c]
  (let [l (lum c)
        n (m/min (.x c) (.y c) (.z c))
        x (m/max (.x c) (.y c) (.z c))
        c (if (m/< n 0.0) (scale-around c l (m// l (m/- l n))) c)]
    (if (m/> x 255.0) (scale-around c l (m// (m/- 255.0 l) (m/- x l))) c)))

(defn- set-lum
  "W3C `SetLum`: `c` shifted to luminance `l`, then clipped."
  ^Vec4 [^Vec4 c ^double l]
  (let [d (m/- l (lum c))]
    (clip-color (Vec4. (m/+ (.x c) d) (m/+ (.y c) d) (m/+ (.z c) d) (.w c)))))

(defn- sat
  "W3C `Sat`: largest minus smallest channel."
  ^double [^Vec4 c]
  (m/- (m/max (.x c) (.y c) (.z c)) (m/min (.x c) (.y c) (.z c))))

(defn- set-sat
  "W3C `SetSat`: channels rescaled so that the smallest is 0 and the largest `s`; a gray gives black."
  ^Vec4 [^Vec4 c ^double s]
  (let [mn (m/min (.x c) (.y c) (.z c))
        mx (m/max (.x c) (.y c) (.z c))]
    (if (m/> mx mn)
      (let [f (m// s (m/- mx mn))]
        (Vec4. (m/* (m/- (.x c) mn) f) (m/* (m/- (.y c) mn) f) (m/* (m/- (.z c) mn) f) (.w c)))
      (Vec4. 0.0 0.0 0.0 (.w c)))))

(defn- whole-color-op
  "`(f cb cs)` on colors, composited with [[alpha-blending]]."
  [f cb cs]
  (let [cb (c/to-color cb)
        cs (c/to-color cs)]
    (alpha-blending cb cs (f cb cs))))

(defn w3c-hue
  "W3C hue mode: the hue of the source with the saturation and luminance of the backdrop.

  Formula: `SetLum(SetSat(cs, Sat(cb)), Lum(cb))` with W3C `Lum = 0.3 r + 0.59 g + 0.11 b`. W3C Compositing Level 1 `hue`; composited with the colors' alpha by [[alpha-blending]].

  - `cb` backdrop color, anything accepted by `clojure2d.color/to-color`
  - `cs` source color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. See also [[hue]] (HSB), [[w3c-saturation]], [[w3c-color]], [[w3c-luminosity]]."
  [cb cs]
  (whole-color-op (fn [cb cs] (set-lum (set-sat cs (sat cb)) (lum cb))) cb cs))

(defn w3c-saturation
  "W3C saturation mode: the saturation of the source with the hue and luminance of the backdrop.

  Formula: `SetLum(SetSat(cb, Sat(cs)), Lum(cb))`. W3C Compositing Level 1 `saturation`; composited with the colors' alpha by [[alpha-blending]].

  - `cb` backdrop color, anything accepted by `clojure2d.color/to-color`
  - `cs` source color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. See also [[saturation]] (HSB), [[w3c-hue]], [[w3c-color]], [[w3c-luminosity]]."
  [cb cs]
  (whole-color-op (fn [cb cs] (set-lum (set-sat cb (sat cs)) (lum cb))) cb cs))

(defn w3c-color
  "W3C color mode: the hue and saturation of the source with the luminance of the backdrop.

  Formula: `SetLum(cs, Lum(cb))`. W3C Compositing Level 1 `color`; composited with the colors' alpha by [[alpha-blending]].

  - `cb` backdrop color, anything accepted by `clojure2d.color/to-color`
  - `cs` source color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. See also [[color]] (HSB), [[w3c-luminosity]], [[w3c-hue]], [[w3c-saturation]]."
  [cb cs]
  (whole-color-op (fn [cb cs] (set-lum cs (lum cb))) cb cs))

(defn w3c-luminosity
  "W3C luminosity mode: the luminance of the source with the hue and saturation of the backdrop.

  Formula: `SetLum(cb, Lum(cs))`. W3C Compositing Level 1 `luminosity`; composited with the colors' alpha by [[alpha-blending]].

  - `cb` backdrop color, anything accepted by `clojure2d.color/to-color`
  - `cs` source color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. See also [[luminosity]] (HSB), [[w3c-color]], [[w3c-hue]], [[w3c-saturation]]."
  [cb cs]
  (whole-color-op (fn [cb cs] (set-lum cb (lum cs))) cb cs))

(defn darker-color
  "Darker color mode (Photoshop): the whole color with the lower luminance.

  Compares W3C `Lum = 0.3 r + 0.59 g + 0.11 b`; on a tie the backdrop is kept. Composited with the colors' alpha by [[alpha-blending]].

  - `cb` backdrop color, anything accepted by `clojure2d.color/to-color`
  - `cs` source color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. See also [[lighter-color]], [[darken]] (per channel)."
  [cb cs]
  (whole-color-op (fn [cb cs] (if (m/< (lum cs) (lum cb)) cs cb)) cb cs))

(defn lighter-color
  "Lighter color mode (Photoshop): the whole color with the higher luminance.

  Compares W3C `Lum = 0.3 r + 0.59 g + 0.11 b`; on a tie the backdrop is kept. Composited with the colors' alpha by [[alpha-blending]].

  - `cb` backdrop color, anything accepted by `clojure2d.color/to-color`
  - `cs` source color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. See also [[darker-color]], [[lighten]] (per channel)."
  [cb cs]
  (whole-color-op (fn [cb cs] (if (m/> (lum cs) (lum cb)) cs cb)) cb cs))

(def ^{:doc "Whole-color modes by name: keyword to `(f cb cs)` returning a composited `Vec4`.

  HSB channel blends `:hue`, `:saturation`, `:luminosity`, `:color`; W3C non-separable modes `:w3c-hue`, `:w3c-saturation`, `:w3c-color`, `:w3c-luminosity`; `:darker-color`, `:lighter-color`; channel copies `:ch0`, `:ch1`, `:ch2` (these also take a colorspace as the first argument). Per-channel modes are in [[blends]]."}
  color-blends {:hue hue
                :saturation saturation
                :luminosity luminosity
                :color color
                :w3c-hue w3c-hue
                :w3c-saturation w3c-saturation
                :w3c-color w3c-color
                :w3c-luminosity w3c-luminosity
                :darker-color darker-color
                :lighter-color lighter-color
                :ch0 ch0
                :ch1 ch1
                :ch2 ch2})
