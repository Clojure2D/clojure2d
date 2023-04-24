(ns clojure2d.color.whitepoints
  (:require [fastmath.core :as m]
            [fastmath.vector :as v])
  (:import [fastmath.vector Vec2 Vec3]
           [org.apache.commons.math3.linear RealMatrix
            Array2DRowRealMatrix MatrixUtils ArrayRealVector DiagonalMatrix]))

#_(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; https://github.com/colour-science/colour/blob/develop/colour/colorimetry/datasets/illuminants/chromaticity_coordinates.py

(def illuminants
  {:CIE2  {:A             (Vec2. 0.44758 0.40745)
           :B             (Vec2. 0.34842 0.35161)
           :C             (Vec2. 0.31006 0.31616)
           :D50           (Vec2. 0.34567 0.35851)
           :D55           (Vec2. 0.33243 0.34744)
           :D60           (Vec2. 0.321616709705268 0.337619916550817)
           :D65           (Vec2. 0.312727 0.329023) ;; fixed
           :D75           (Vec2. 0.29903 0.31488)
           :E             (Vec2. m/THIRD m/THIRD)
           :FL1           (Vec2. 0.31310 0.33710)
           :FL2           (Vec2. 0.37210 0.37510)
           :FL3           (Vec2. 0.40910 0.39410)
           :FL4           (Vec2. 0.44020 0.40310)
           :FL5           (Vec2. 0.31380 0.34520)
           :FL6           (Vec2. 0.37790 0.38820)
           :FL7           (Vec2. 0.31290 0.32920)
           :FL8           (Vec2. 0.34580 0.35860)
           :FL9           (Vec2. 0.37410 0.37270)
           :FL10          (Vec2. 0.34580 0.35880)
           :FL11          (Vec2. 0.38050 0.37690)
           :FL12          (Vec2. 0.43700 0.40420)
           :FL3-1         (Vec2. 0.44070 0.40330)
           :FL3-2         (Vec2. 0.38080 0.37340)
           :FL3-3         (Vec2. 0.31530 0.34390)
           :FL3-4         (Vec2. 0.44290 0.40430)
           :FL3-5         (Vec2. 0.37490 0.36720)
           :FL3-6         (Vec2. 0.34880 0.36000)
           :FL3-7         (Vec2. 0.43840 0.40450)
           :FL3-8         (Vec2. 0.38200 0.38320)
           :FL3-9         (Vec2. 0.34990 0.35910)
           :FL3-10        (Vec2. 0.34550 0.35600)
           :FL3-11        (Vec2. 0.32450 0.34340)
           :FL3-12        (Vec2. 0.43770 0.40370)
           :FL3-13        (Vec2. 0.38300 0.37240)
           :FL3-14        (Vec2. 0.34470 0.36090)
           :FL3-15        (Vec2. 0.31270 0.32880)
           :HP1           (Vec2. 0.53300 0.4150)
           :HP2           (Vec2. 0.47780 0.41580)
           :HP3           (Vec2. 0.43020 0.40750)
           :HP4           (Vec2. 0.38120 0.37970)
           :HP5           (Vec2. 0.37760 0.37130)
           :LED-B1        (Vec2. 0.45600 0.40780)
           :LED-B2        (Vec2. 0.43570 0.40120)
           :LED-B3        (Vec2. 0.37560 0.37230)
           :LED-B4        (Vec2. 0.34220 0.35020)
           :LED-B5        (Vec2. 0.31180 0.32360)
           :LED-BH1       (Vec2. 0.44740 0.40660)
           :LED-RGB1      (Vec2. 0.45570 0.42110)
           :LED-V1        (Vec2. 0.45480 0.40440)
           :LED-V2        (Vec2. 0.37810 0.37750)
           :ID65          (Vec2. 0.310656625403120 0.330663091836953)
           :ID50          (Vec2. 0.343211370103531 0.360207541805137)
           :ACES          (Vec2. 0.32168, 0.33767)
           :BLACKMAGIC-WG (Vec2. 0.3127170, 0.3290312) ;; Blackmagic Wide Gamut
           :DCI-P3        (Vec2. 0.31400, 0.35100)
           :ICC-D50       (Vec2. 0.345702914918791, 0.358538596679933)
           ;; ISO 7589
           :ISO-PD        (Vec2. 0.332039098470978, 0.347263885596614) ;; Photographics Daylight
           :ISO-SD        (Vec2. 0.333818313227557, 0.353436231513603) ;; Sensitometric Daylight
           :ISO-ST        (Vec2. 0.430944089109761, 0.403585442674295) ;; Studio Tungsten
           :ISO-SST       (Vec2. 0.431418223648390, 0.407471441950342) ;; Sensitometric Studio Tungsten
           :ISO-P         (Vec2. 0.411146015714843, 0.393719378241161) ;; Photoflood
           :ISO-SP        (Vec2. 0.412024776908998, 0.398177410548532) ;; Sensitometric Photoflood
           :ISO-SPR       (Vec2. 0.412087967973680, 0.421104984758526)} ;; Sensitometric Printer
   :CIE10 {:A        (Vec2. 0.45117 0.40594)
           :B        (Vec2. 0.34980 0.35270)
           :C        (Vec2. 0.31039 0.31905)
           :D50      (Vec2. 0.34773 0.35952)
           :D55      (Vec2. 0.33412 0.34877)
           :D60      (Vec2. 0.322986926715820 0.339275732345997)
           :D65      (Vec2. 0.31381 0.33098)
           :D75      (Vec2. 0.29968 0.31740)
           :E        (Vec2. m/THIRD m/THIRD)
           :FL1      (Vec2. 0.31811 0.33559)
           :FL2      (Vec2. 0.37925 0.36733)
           :FL3      (Vec2. 0.41761 0.38324)
           :FL4      (Vec2. 0.44920 0.39074)
           :FL5      (Vec2. 0.31975 0.34246)
           :FL6      (Vec2. 0.38660 0.37847)
           :FL7      (Vec2. 0.31569 0.32960)
           :FL8      (Vec2. 0.34902 0.35939)
           :FL9      (Vec2. 0.37829 0.37045)
           :FL10     (Vec2. 0.35090 0.35444)
           :FL11     (Vec2. 0.38541 0.37123)
           :FL12     (Vec2. 0.44256 0.39717)
           :FL3-1    (Vec2. 0.449830684010003 0.390231404321266)
           :FL3-2    (Vec2. 0.386924116672933 0.365756034732821)
           :FL3-3    (Vec2. 0.321176986855865 0.340501092654981)
           :FL3-4    (Vec2. 0.448121275113995 0.397077112142482)
           :FL3-5    (Vec2. 0.377814166608895 0.366625766963060)
           :FL3-6    (Vec2. 0.351976478983504 0.361094432889677)
           :FL3-7    (Vec2. 0.444309208810922 0.396791387314871)
           :FL3-8    (Vec2. 0.387588931999771 0.376305569410173)
           :FL3-9    (Vec2. 0.354688990710449 0.353445033593383)
           :FL3-10   (Vec2. 0.349344792334400 0.354984421140869)
           :FL3-11   (Vec2. 0.329267975695120 0.338865386643537)
           :FL3-12   (Vec2. 0.442252080438001 0.401220551071252)
           :FL3-13   (Vec2. 0.386275268780817 0.374283190950586)
           :FL3-14   (Vec2. 0.347255078638291 0.366808242504180)
           :FL3-15   (Vec2. 0.314613997909246 0.333377149377113)
           :HP1      (Vec2. 0.543334600247307 0.405289298480431)
           :HP2      (Vec2. 0.482647330648721 0.410815644179685)
           :HP3      (Vec2. 0.435560034503954 0.398801084399711)
           :HP4      (Vec2. 0.385193641123543 0.368275479241015)
           :HP5      (Vec2. 0.380316415606638 0.366617114797851)
           :LED-B1   (Vec2. 0.462504966271043 0.403041801546906)
           :LED-B2   (Vec2. 0.442119475258745 0.396633702892576)
           :LED-B3   (Vec2. 0.380851979328052 0.368518548904765)
           :LED-B4   (Vec2. 0.348371362473402 0.345065503264192)
           :LED-B5   (Vec2. 0.316916877024753 0.322060276350364)
           :LED-BH1  (Vec2. 0.452772610754910 0.400032462750000)
           :LED-RGB1 (Vec2. 0.457036370583652 0.425381348780888)
           :LED-V1   (Vec2. 0.453602699414564 0.398199587905174)
           :LED-V2   (Vec2. 0.377728483834020 0.374512315539769)
           :ID65     (Vec2. 0.312074043269908 0.332660121024630)
           :ID50     (Vec2. 0.345621427535976 0.361228962209198)
           ;; ISO 7589
           :ISO-PD        (Vec2. 0.333716908394534, 0.348592494683065) ;; Photographics Daylight
           :ISO-SD        (Vec2. 0.336125906007630, 0.354997062476417) ;; Sensitometric Daylight
           :ISO-ST        (Vec2. 0.434575926493196, 0.402219691745325) ;; Studio Tungsten
           :ISO-SST       (Vec2. 0.435607674215215, 0.406129244796761) ;; Sensitometric Studio Tungsten
           :ISO-P         (Vec2. 0.414144647169611, 0.392458587686395) ;; Photoflood
           :ISO-SP        (Vec2. 0.415625819190627, 0.397002292994179) ;; Sensitometric Photoflood
           :ISO-SPR       (Vec2. 0.418841052206998, 0.418695130974955) ;; Sensitometric Printer
           }})

(defn xy->XZ
  "Convert xy coordinates (from Yxy) to XZ coordinates (from XYZ)."
  (^Vec2 [^Vec2 xy] (xy->XZ (.x xy) (.y xy)))
  (^Vec2 [^double x ^double y]
   (-> (Vec2. x (- 1.0 x y))
       (v/div y))))

(defn XZ->xy
  (^Vec2 [^Vec2 XZ] (XZ->xy (.x XZ) (.y XZ)))
  (^Vec2 [^double X ^double Z]
   (-> (Vec2. X Z)
       (v/div (+ 1.0 X Z)))))

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

(defn tristimulus
  "Create XYZ color for given [observer, illuminant] pair, or x,y coordinates.

  * observer: :CIE2 or CIE10
  * illuminant: :A, :B, :D50, :D65 etc, see [[illuminants]] var.

  Default is `[:CIE2 :D65]`."
  ([] (tristimulus [:CIE2 :D65]))
  ([observer-illuminant-or-vec2]
   (-> (if (instance? Vec2 observer-illuminant-or-vec2)
         observer-illuminant-or-vec2
         (get-in illuminants observer-illuminant-or-vec2)) xy->XZ add-Y))
  ([^double x ^double y]
   (-> (Vec2. x y) xy->XZ add-Y)))

;;

(defn- columns->matrix
  ^RealMatrix [c1 c2 c3]
  (-> (map vector c1 c2 c3) m/seq->double-double-array Array2DRowRealMatrix.))

(defn- rows->matrix
  ^RealMatrix [r1 r2 r3]
  (-> [r1 r2 r3] m/seq->double-double-array Array2DRowRealMatrix.))

(defn- diagonal->matrix
  ^RealMatrix [d1 d2 d3]
  (DiagonalMatrix. (m/seq->double-array [d1 d2 d3]))) 

(defn- inverse-matrix
  ^RealMatrix [m]
  (MatrixUtils/inverse m))

(defn- mv
  ^ArrayRealVector [^RealMatrix m ^ArrayRealVector v]
  (.operate m (ArrayRealVector. (m/seq->double-array v))))

(defn- mm
  ^RealMatrix [^RealMatrix m1 ^RealMatrix m2]
  (.multiply m1 m2))

(def chromatic-adaptation-methods
  (let [I         (diagonal->matrix 1.0 1.0 1.0)
        bradford  (rows->matrix [0.8951000  0.2664000 -0.1614000]
                                [-0.7502000  1.7135000  0.0367000]
                                [0.0389000 -0.0685000  1.0296000])
        von-kries (rows->matrix [0.4002400  0.7076000 -0.0808100]
                                [-0.2263000  1.1653200  0.0457000]
                                [0.0000000  0.0000000  0.9182200])]
    {:xy-scaling [I I]
     :bradford   [bradford (inverse-matrix bradford)]
     :von-kries  [von-kries (inverse-matrix von-kries)]}))

(defn- chromatic-adaptation-matrix
  ([source-wp destination-wp] (chromatic-adaptation-matrix :von-kries source-wp destination-wp))
  ([adaptation-method source-wp destination-wp]
   (let [ws (tristimulus source-wp)
         wd (tristimulus destination-wp)
         [Ma Ma-1] (chromatic-adaptation-methods adaptation-method)
         ^doubles s (.getDataRef (mv Ma ws))
         ^doubles d (.getDataRef (mv Ma wd))
         diag (diagonal->matrix (/ (aget d 0) (aget s 0))
                                (/ (aget d 1) (aget s 1))
                                (/ (aget d 2) (aget s 2)))
         M (mm Ma-1 (mm diag Ma))]
     [M (inverse-matrix M)])))

(chromatic-adaptation-matrix :von-kries [:CIE2 :D65] [:CIE2 :D50])

(def rgbs
  {:sRGB {:red (Vec2. 0.64 0.33) :green (Vec2. 0.3 0.6) :blue (Vec2. 0.15 0.06)}})

;; generate XYZ conversion matrix
(comment (let [r (tristimulus (get-in rgbs [:sRGB :red]))
               g (tristimulus (get-in rgbs [:sRGB :green]))
               b (tristimulus (get-in rgbs [:sRGB :blue]))
               w (tristimulus [:CIE2 :D65])
               ^doubles s (.getDataRef (mv (inverse-matrix (columns->matrix r g b)) w))]
           (map vector
                (v/mult r (aget s 0))
                (v/mult g (aget s 1))
                (v/mult b (aget s 2)))))
;; => ([0.4124585781796624 0.35757553616579035 0.1804374332363275]
;;     [0.21267395437388842 0.7151510723315807 0.072174973294531]
;;     [0.019333995852171656 0.11919184538859676 0.9503038150446582])

