(ns clojure2d.color.rgb
  (:require [clojure2d.color.whitepoints :as wp]
            [fastmath.matrix :as mat]
            [fastmath.core :as m])
  (:import [fastmath.matrix Mat3x3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn srgb-to-linear
  "Gamma correction (gamma=2.4), darken"
  ^double [^double v]
  (if (m/> v 0.040449936)
    (m/pow (m// (m/+ 0.055 v) 1.055) 2.4)
    (m// v 12.92)))

(defn linear-to-srgb
  "Gamma correction (gamma=1/2.4), lighten"
  ^double [^double v]
  (if (m/> v 0.0031308)
    (m/- (m/* 1.055 (m/pow v 0.4166666666666667)) 0.055)
    (m/* v 12.92)))

(defn ->gamma
  "Create gamma correction function"
  [^double e]
  (fn ^double [^double x]
    (m/spow x e)))

;; memoized version
(def ^:private ->mgamma (memoize ->gamma))

(def ^{:private true :const true :tag 'double} CIEEpsilon (/ 216.0 24389.0))
(def ^{:private true :const true :tag 'double} CIEK (/ 24389.0 27.0))

(defn to-L*
  "CIE correction, L*"
  ^double [^double v]
  (m// (if (> v CIEEpsilon)
         (m/- (m/* 116.0 (m/cbrt v)) 16.0)
         (* v CIEK)) 100.0))

(defn from-L*
  "Inverse CIE correction, L*"
  ^double [^double v]
  (let [v (m// (m/+ (m/* v 100.0) 16.0) 116.0)
        v3 (* v v v)]
    (if (> v3 CIEEpsilon)
      v3
      (/ (- (* 116.0 v) 16.0) CIEK))))

(defn to-ACEScc
  ^double [^double v]
  (cond
    (m/not-pos? v) -0.35844748858447484
    (m/>= v 3.0517578125E-5) (m// (m/+ (m/log2 v) 9.72) 17.52)
    :else (m// (m/+ (m/log2 (m/+ 1.52587890625E-5 (m/* 0.5 v))) 9.72) 17.52)))

(defn from-ACEScc
  ^double [^double v]
  (cond
    (m/<= v -0.3013698630136986) (m/* 2.0 (m/- (m/pow 2.0 (m/- (m/* v 17.52) 9.72)) 1.52587890625E-5))
    (m/>= v 1.4679963120447153) 65504.0
    :else (m/pow 2.0 (m/- (m/* v 17.52) 9.72))))

(defn to-ACEScct
  ^double [^double v]
  (if (m/<= v 0.0078125)
    (m/+ (m/* v 10.5402377416545) 0.0729055341958355)
    (m// (m/+ (m/log2 v) 9.72) 17.52)))

(defn from-ACEScct
  ^double [^double v]
  (cond
    (m/<= v 0.155251141552511) (m// (m/- v 0.0729055341958355) 10.5402377416545)
    (m/>= v 1.4679963120447153) 65504.0
    :else (m/pow 2.0 (m/- (m/* v 17.52) 9.72))))

(defn to-Blackmagic-WG
  ^double [^double v]
  (if (m/< v 0.005)
    (m/+ (m/* v 8.283605932402494) 0.09246575342465753)
    (m/+ 0.5300133392291939 (m/* 0.08692876065491224 (m/log (m/+ v 0.005494072432257808))))))

(defn from-Blackmagic-WG
  ^double [^double v]
  (if (m/< v 0.13388378308667)
    (m// (m/- v 0.09246575342465753) 8.283605932402494)
    (m/- (m/exp (m// (m/- v 0.5300133392291939) 0.08692876065491224)) 0.005494072432257808)))

(defn to-BT2020
  ^double [^double v]
  (if (m/< v 0.018053968510807)
    (m/* 4.5 v)
    (m/- (m/* 1.09929682680944 (m/spow v 0.45)) 0.09929682680944)))

(defn from-BT2020
  ^double [^double v]
  (if (m/< v 0.08124285829863397)
    (m// v 4.5)
    (m/spow (m// (m/+ v 0.09929682680944) 1.09929682680944) 2.2222222222222222)))

;;

(defn rgb-data->matrix
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
  [red green blue whitepoint to from]
  (let [[m1 m2] (rgb-data->matrix red green blue whitepoint)]
    {:red red :green green :blue blue
     :rgb-to-xyz m1
     :xyz-to-rgb m2
     :whitepoint whitepoint
     :to to
     :from from}))

(def rgbs (let [m {:ACES2065-1 (rgb-data->map   [0.73470, 0.26530],
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
                 :Xtreme-RGB (rgb-data->map [1.0 0.0]
                                            [0.0 1.0]
                                            [0.0 0.0]
                                            wp/CIE-2-D50
                                            (->mgamma 0.45454545454545453)
                                            (->mgamma 2.2))}]
          (-> m
              (assoc :ITU-R-BT470-525 (:NTSC m))
              (assoc :ITU-R-BT470-625 (:PAL-SECAM m)))))

