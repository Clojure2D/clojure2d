(ns clojure2d.color.whitepoints
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [fastmath.matrix :as mat])
  (:import [fastmath.vector Vec2 Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; https://github.com/colour-science/colour/blob/develop/colour/colorimetry/datasets/illuminants/chromaticity_coordinates.py

;; CIE 2 1931

(def CIE2-A              (Vec2. 0.44758 0.40745))
(def CIE2-B              (Vec2. 0.34842 0.35161))
(def CIE2-C              (Vec2. 0.31006 0.31616))
(def CIE2-D50            (Vec2. 0.34567 0.35851))
(def CIE2-D55            (Vec2. 0.33243 0.34744))
(def CIE2-D60            (Vec2. 0.321616709705268 0.337619916550817))
(def CIE2-D65            (Vec2. 0.312727 0.329023)) ;; better precision
(def CIE2-D75            (Vec2. 0.29903 0.31488))
(def CIE2-E              (Vec2. 0.3333333333333333 0.3333333333333333))
(def CIE2-FL1            (Vec2. 0.31310 0.33710))
(def CIE2-FL2            (Vec2. 0.37210 0.37510))
(def CIE2-FL3            (Vec2. 0.40910 0.39410))
(def CIE2-FL4            (Vec2. 0.44020 0.40310))
(def CIE2-FL5            (Vec2. 0.31380 0.34520))
(def CIE2-FL6            (Vec2. 0.37790 0.38820))
(def CIE2-FL7            (Vec2. 0.31290 0.32920))
(def CIE2-FL8            (Vec2. 0.34580 0.35860))
(def CIE2-FL9            (Vec2. 0.37410 0.37270))
(def CIE2-FL10           (Vec2. 0.34580 0.35880))
(def CIE2-FL11           (Vec2. 0.38050 0.37690))
(def CIE2-FL12           (Vec2. 0.43700 0.40420))
(def CIE2-FL3-1          (Vec2. 0.44070 0.40330))
(def CIE2-FL3-2          (Vec2. 0.38080 0.37340))
(def CIE2-FL3-3          (Vec2. 0.31530 0.34390))
(def CIE2-FL3-4          (Vec2. 0.44290 0.40430))
(def CIE2-FL3-5          (Vec2. 0.37490 0.36720))
(def CIE2-FL3-6          (Vec2. 0.34880 0.36000))
(def CIE2-FL3-7          (Vec2. 0.43840 0.40450))
(def CIE2-FL3-8          (Vec2. 0.38200 0.38320))
(def CIE2-FL3-9          (Vec2. 0.34990 0.35910))
(def CIE2-FL3-10         (Vec2. 0.34550 0.35600))
(def CIE2-FL3-11         (Vec2. 0.32450 0.34340))
(def CIE2-FL3-12         (Vec2. 0.43770 0.40370))
(def CIE2-FL3-13         (Vec2. 0.38300 0.37240))
(def CIE2-FL3-14         (Vec2. 0.34470 0.36090))
(def CIE2-FL3-15         (Vec2. 0.31270 0.32880))
(def CIE2-HP1            (Vec2. 0.53300 0.4150))
(def CIE2-HP2            (Vec2. 0.47780 0.41580))
(def CIE2-HP3            (Vec2. 0.43020 0.40750))
(def CIE2-HP4            (Vec2. 0.38120 0.37970))
(def CIE2-HP5            (Vec2. 0.37760 0.37130))
(def CIE2-LED-B1         (Vec2. 0.45600 0.40780))
(def CIE2-LED-B2         (Vec2. 0.43570 0.40120))
(def CIE2-LED-B3         (Vec2. 0.37560 0.37230))
(def CIE2-LED-B4         (Vec2. 0.34220 0.35020))
(def CIE2-LED-B5         (Vec2. 0.31180 0.32360))
(def CIE2-LED-BH1        (Vec2. 0.44740 0.40660))
(def CIE2-LED-RGB1       (Vec2. 0.45570 0.42110))
(def CIE2-LED-V1         (Vec2. 0.45480 0.40440))
(def CIE2-LED-V2         (Vec2. 0.37810 0.37750))
(def CIE2-ID65           (Vec2. 0.310656625403120 0.330663091836953))
(def CIE2-ID50           (Vec2. 0.343211370103531 0.360207541805137))
(def CIE2-ACES           (Vec2. 0.32168, 0.33767))
(def CIE2-BLACKMAGIC-WG  (Vec2. 0.3127170, 0.3290312)) ;; Blackmagic Wide Gamut
(def CIE2-DCI-P3         (Vec2. 0.31400, 0.35100))
(def CIE2-ICC-D50       (Vec2. 0.345702914918791, 0.358538596679933))
(def CIE2-ISO-PD        (Vec2. 0.332039098470978, 0.347263885596614)) ;; Photographics Daylight
(def CIE2-ISO-SD        (Vec2. 0.333818313227557, 0.353436231513603)) ;; Sensitometric Daylight
(def CIE2-ISO-ST        (Vec2. 0.430944089109761, 0.403585442674295)) ;; Studio Tungsten
(def CIE2-ISO-SST       (Vec2. 0.431418223648390, 0.407471441950342)) ;; Sensitometric Studio Tungsten
(def CIE2-ISO-PF        (Vec2. 0.411146015714843, 0.393719378241161)) ;; Photoflood
(def CIE2-ISO-SPF       (Vec2. 0.412024776908998, 0.398177410548532)) ;; Sensitometric Photoflood
(def CIE2-ISO-SP        (Vec2. 0.412087967973680, 0.421104984758526)) ;; Sensitometric Printer
(def CIE2-PLASA         (Vec2. 0.4254, 0.4044))

;; CIE 10 1964
(def CIE10-A        (Vec2. 0.45117 0.40594))
(def CIE10-B        (Vec2. 0.34980 0.35270))
(def CIE10-C        (Vec2. 0.31039 0.31905))
(def CIE10-D50      (Vec2. 0.34773 0.35952))
(def CIE10-D55      (Vec2. 0.33412 0.34877))
(def CIE10-D60      (Vec2. 0.322986926715820 0.339275732345997))
(def CIE10-D65      (Vec2. 0.31381 0.33098))
(def CIE10-D75      (Vec2. 0.29968 0.31740))
(def CIE10-E        (Vec2. 0.3333333333333333 0.3333333333333333))
(def CIE10-FL1      (Vec2. 0.31811 0.33559))
(def CIE10-FL2      (Vec2. 0.37925 0.36733))
(def CIE10-FL3      (Vec2. 0.41761 0.38324))
(def CIE10-FL4      (Vec2. 0.44920 0.39074))
(def CIE10-FL5      (Vec2. 0.31975 0.34246))
(def CIE10-FL6      (Vec2. 0.38660 0.37847))
(def CIE10-FL7      (Vec2. 0.31569 0.32960))
(def CIE10-FL8      (Vec2. 0.34902 0.35939))
(def CIE10-FL9      (Vec2. 0.37829 0.37045))
(def CIE10-FL10     (Vec2. 0.35090 0.35444))
(def CIE10-FL11     (Vec2. 0.38541 0.37123))
(def CIE10-FL12     (Vec2. 0.44256 0.39717))
(def CIE10-FL3-1    (Vec2. 0.449830684010003 0.390231404321266))
(def CIE10-FL3-2    (Vec2. 0.386924116672933 0.365756034732821))
(def CIE10-FL3-3    (Vec2. 0.321176986855865 0.340501092654981))
(def CIE10-FL3-4    (Vec2. 0.448121275113995 0.397077112142482))
(def CIE10-FL3-5    (Vec2. 0.377814166608895 0.366625766963060))
(def CIE10-FL3-6    (Vec2. 0.351976478983504 0.361094432889677))
(def CIE10-FL3-7    (Vec2. 0.444309208810922 0.396791387314871))
(def CIE10-FL3-8    (Vec2. 0.387588931999771 0.376305569410173))
(def CIE10-FL3-9    (Vec2. 0.354688990710449 0.353445033593383))
(def CIE10-FL3-10   (Vec2. 0.349344792334400 0.354984421140869))
(def CIE10-FL3-11   (Vec2. 0.329267975695120 0.338865386643537))
(def CIE10-FL3-12   (Vec2. 0.442252080438001 0.401220551071252))
(def CIE10-FL3-13   (Vec2. 0.386275268780817 0.374283190950586))
(def CIE10-FL3-14   (Vec2. 0.347255078638291 0.366808242504180))
(def CIE10-FL3-15   (Vec2. 0.314613997909246 0.333377149377113))
(def CIE10-HP1      (Vec2. 0.543334600247307 0.405289298480431))
(def CIE10-HP2      (Vec2. 0.482647330648721 0.410815644179685))
(def CIE10-HP3      (Vec2. 0.435560034503954 0.398801084399711))
(def CIE10-HP4      (Vec2. 0.385193641123543 0.368275479241015))
(def CIE10-HP5      (Vec2. 0.380316415606638 0.366617114797851))
(def CIE10-LED-B1   (Vec2. 0.462504966271043 0.403041801546906))
(def CIE10-LED-B2   (Vec2. 0.442119475258745 0.396633702892576))
(def CIE10-LED-B3   (Vec2. 0.380851979328052 0.368518548904765))
(def CIE10-LED-B4   (Vec2. 0.348371362473402 0.345065503264192))
(def CIE10-LED-B5   (Vec2. 0.316916877024753 0.322060276350364))
(def CIE10-LED-BH1  (Vec2. 0.452772610754910 0.400032462750000))
(def CIE10-LED-RGB1 (Vec2. 0.457036370583652 0.425381348780888))
(def CIE10-LED-V1   (Vec2. 0.453602699414564 0.398199587905174))
(def CIE10-LED-V2   (Vec2. 0.377728483834020 0.374512315539769))
(def CIE10-ID65     (Vec2. 0.312074043269908 0.332660121024630))
(def CIE10-ID50     (Vec2. 0.345621427535976 0.361228962209198))
(def CIE10-ISO-PD        (Vec2. 0.333716908394534, 0.348592494683065)) ;; Photographics Daylight
(def CIE10-ISO-SD        (Vec2. 0.336125906007630, 0.354997062476417)) ;; Sensitometric Daylight
(def CIE10-ISO-ST        (Vec2. 0.434575926493196, 0.402219691745325)) ;; Studio Tungsten
(def CIE10-ISO-SST       (Vec2. 0.435607674215215, 0.406129244796761)) ;; Sensitometric Studio Tungsten
(def CIE10-ISO-PF         (Vec2. 0.414144647169611, 0.392458587686395)) ;; Photoflood
(def CIE10-ISO-SPF        (Vec2. 0.415625819190627, 0.397002292994179)) ;; Sensitometric Photoflood
(def CIE10-ISO-SP       (Vec2. 0.418841052206998, 0.418695130974955)) ;; Sensitometric Printer

(def illuminants
  {:CIE2 {:A              CIE2-A     
          :B              CIE2-B     
          :C              CIE2-C     
          :D50            CIE2-D50   
          :D55            CIE2-D55   
          :D60            CIE2-D60   
          :D65            CIE2-D65    ;; better precision
          :D75            CIE2-D75   
          :E              CIE2-E     
          :FL1            CIE2-FL1   
          :FL2            CIE2-FL2   
          :FL3            CIE2-FL3   
          :FL4            CIE2-FL4   
          :FL5            CIE2-FL5   
          :FL6            CIE2-FL6   
          :FL7            CIE2-FL7   
          :FL8            CIE2-FL8   
          :FL9            CIE2-FL9   
          :FL10           CIE2-FL10  
          :FL11           CIE2-FL11  
          :FL12           CIE2-FL12  
          :FL3-1          CIE2-FL3-1 
          :FL3-2          CIE2-FL3-2 
          :FL3-3          CIE2-FL3-3 
          :FL3-4          CIE2-FL3-4 
          :FL3-5          CIE2-FL3-5 
          :FL3-6          CIE2-FL3-6 
          :FL3-7          CIE2-FL3-7 
          :FL3-8          CIE2-FL3-8 
          :FL3-9          CIE2-FL3-9 
          :FL3-10         CIE2-FL3-10
          :FL3-11         CIE2-FL3-11
          :FL3-12         CIE2-FL3-12
          :FL3-13         CIE2-FL3-13
          :FL3-14         CIE2-FL3-14
          :FL3-15         CIE2-FL3-15
          :HP1            CIE2-HP1   
          :HP2            CIE2-HP2   
          :HP3            CIE2-HP3   
          :HP4            CIE2-HP4   
          :HP5            CIE2-HP5   
          :LED-B1         CIE2-LED-B1
          :LED-B2         CIE2-LED-B2
          :LED-B3         CIE2-LED-B3
          :LED-B4         CIE2-LED-B4
          :LED-B5         CIE2-LED-B5
          :LED-BH1        CIE2-LED-BH1      
          :LED-RGB1       CIE2-LED-RGB1     
          :LED-V1         CIE2-LED-V1       
          :LED-V2         CIE2-LED-V2       
          :ID65           CIE2-ID65         
          :ID50           CIE2-ID50         
          :ACES           CIE2-ACES         
          :BLACKMAGIC-WG  CIE2-BLACKMAGIC-WG ;; Blackmagic Wide Gamut
          :DCI-P3         CIE2-DCI-P3       
          :ICC-D50        CIE2-ICC-D50      
          :ISO-PD         CIE2-ISO-PD        ;; Photographics Daylight
          :ISO-SD         CIE2-ISO-SD        ;; Sensitometric Daylight
          :ISO-ST         CIE2-ISO-ST        ;; Studio Tungsten
          :ISO-SST        CIE2-ISO-SST       ;; Sensitometric Studio Tungsten
          :ISO-PF         CIE2-ISO-PF        ;; Photoflood
          :ISO-SPF        CIE2-ISO-SPF       ;; Sensitometric Photoflood
          :ISO-SP         CIE2-ISO-SP        ;; Sensitometric Printer
          :PLASA          CIE2-PLASA}
   :CIE10 {:A      CIE10-A     
           :B      CIE10-B     
           :C      CIE10-C     
           :D50    CIE10-D50   
           :D55    CIE10-D55   
           :D60    CIE10-D60   
           :D65    CIE10-D65   
           :D75    CIE10-D75   
           :E      CIE10-E     
           :FL1    CIE10-FL1   
           :FL2    CIE10-FL2   
           :FL3    CIE10-FL3   
           :FL4    CIE10-FL4   
           :FL5    CIE10-FL5   
           :FL6    CIE10-FL6   
           :FL7    CIE10-FL7   
           :FL8    CIE10-FL8   
           :FL9    CIE10-FL9   
           :FL10   CIE10-FL10  
           :FL11   CIE10-FL11  
           :FL12   CIE10-FL12  
           :FL3-1  CIE10-FL3-1 
           :FL3-2  CIE10-FL3-2 
           :FL3-3  CIE10-FL3-3 
           :FL3-4  CIE10-FL3-4 
           :FL3-5  CIE10-FL3-5 
           :FL3-6  CIE10-FL3-6 
           :FL3-7  CIE10-FL3-7 
           :FL3-8  CIE10-FL3-8 
           :FL3-9  CIE10-FL3-9 
           :FL3-10 CIE10-FL3-10
           :FL3-11 CIE10-FL3-11
           :FL3-12 CIE10-FL3-12
           :FL3-13 CIE10-FL3-13
           :FL3-14 CIE10-FL3-14
           :FL3-15 CIE10-FL3-15
           :HP1    CIE10-HP1   
           :HP2    CIE10-HP2   
           :HP3    CIE10-HP3   
           :HP4    CIE10-HP4   
           :HP5    CIE10-HP5   
           :LED-B1 CIE10-LED-B1
           :LED-B2 CIE10-LED-B2
           :LED-B3 CIE10-LED-B3
           :LED-B4 CIE10-LED-B4
           :LED-B5 CIE10-LED-B5
           :LED-BH1  CIE10-LED-BH1 
           :LED-RGB1 CIE10-LED-RGB1
           :LED-V1   CIE10-LED-V1  
           :LED-V2   CIE10-LED-V2  
           :ID65     CIE10-ID65    
           :ID50     CIE10-ID50    
           :ISO-PD   CIE10-ISO-PD
           :ISO-SD   CIE10-ISO-SD
           :ISO-ST   CIE10-ISO-ST
           :ISO-SST  CIE10-ISO-SST
           :ISO-PF   CIE10-ISO-PF
           :ISO-SPF  CIE10-ISO-SPF
           :ISO-SP   CIE10-ISO-SP}})

(defn xy->XZ
  "Convert xy chroma coordinates (from Yxy) to XZ coordinates (from XYZ)."
  (^Vec2 [^Vec2 xy] (xy->XZ (.x xy) (.y xy)))
  (^Vec2 [^double x ^double y]
   (-> (Vec2. x (- 1.0 x y))
       (v/div y))))

(defn XZ->xy
  "Convert XZ chroma coordinates (from XYZ) to xy coordinates (from Yxy). "
  (^Vec2 [^Vec2 XZ] (XZ->xy (.x XZ) (.y XZ)))
  (^Vec2 [^double X ^double Z]
   (-> (Vec2. X 1.0)
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
  "Create XYZ color for given [observer, illuminant] pair, predifined var or x,y coordinates.

  * observer: :CIE2 or CIE10
  * illuminant: :A, :B, :D50, :D65 etc, see [[illuminants]] var.

  Default is `CIE2-D65`."
  ([] (tristimulus CIE2-D65))
  ([observer-illuminant-or-vec2]
   (-> (if (instance? Vec2 observer-illuminant-or-vec2)
         observer-illuminant-or-vec2
         (get-in illuminants observer-illuminant-or-vec2)) xy->XZ add-Y))
  ([^double x ^double y]
   (-> (Vec2. x y) xy->XZ add-Y)))

;; http://www.brucelindbloom.com/index.html?Eqn_ChromAdapt.html

(def ^:private chromatic-adaptation-methods
  (let [I         (mat/eye 3)
        bradford  (mat/mat3x3 0.8951000  0.2664000 -0.1614000
                              -0.7502000  1.7135000  0.0367000
                              0.0389000 -0.0685000  1.0296000)
        von-kries (mat/mat3x3 0.4002400  0.7076000 -0.0808100
                              -0.2263000  1.1653200  0.0457000
                              0.0000000  0.0000000  0.9182200)
        sharp (mat/mat3x3 1.2694 -0.0988 -0.1706
                          -0.8364 1.8006 0.0357
                          0.0297 -0.0315 1.0018)
        fairchild (mat/mat3x3 0.8562 0.3372 -0.1934
                              -0.8360 1.8327 0.0033
                              0.0357 -0.0469 1.0112)
        cat97 (mat/mat3x3 0.8951 -0.7502 0.0389
                          0.2664 1.7135 0.0685
                          -0.1614 0.0367 1.0296)
        cat2000 (mat/mat3x3  0.7982 0.3389 -0.1371
                             -0.5918 1.5512 0.0406
                             0.0008 0.0239 0.9753)
        cat02 (mat/mat3x3  0.7328 0.4296 -0.1624
                           -0.7036 1.6975 0.0061
                           0.0030 0.0136 0.9834)
        cat02brill2008 (mat/mat3x3 0.7328 0.4296 -0.1624
                                   -0.7036 1.6975 0.0061
                                   0.0000 0.0000 1.0000)
        cat16 (mat/mat3x3 0.401288 0.650173 -0.051461
                          -0.250268 1.204414 0.045854
                          -0.002079 0.048952 0.953127)
        bianco2010 (mat/mat3x3 0.8752 0.2787 -0.1539
                               -0.8904 1.8709 0.0195
                               -0.0061 0.0162 0.9899)
        bianco2010-pc (mat/mat3x3 0.6489 0.3915 -0.0404
                                  -0.3775 1.3055 0.0720
                                  -0.0271 0.0888 0.9383)]
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
  ([source-wp destination-wp] (chromatic-adaptation-matrix :von-kries source-wp destination-wp))
  ([adaptation-method source-wp destination-wp]
   (let [ws (tristimulus source-wp)
         wd (tristimulus destination-wp)
         [Ma Ma-1] (chromatic-adaptation-methods adaptation-method)
         s (mat/mulv Ma ws)
         d (mat/mulv Ma wd)
         diag (mat/diagonal (v/ediv d s))
         M (mat/mulm Ma-1 (mat/mulm diag Ma))]
     [M (mat/inverse M)])))

(def rgbs
  {:sRGB {:red (Vec2. 0.64 0.33) :green (Vec2. 0.3 0.6) :blue (Vec2. 0.15 0.06)}})

;; generate XYZ conversion matrix
(comment (let [r (tristimulus (get-in rgbs [:sRGB :red]))
               g (tristimulus (get-in rgbs [:sRGB :green]))
               b (tristimulus (get-in rgbs [:sRGB :blue]))
               w (tristimulus [:CIE2 :D65])
               s (mat/mulv (mat/inverse (mat/cols->mat3x3 r g b)) w)]
           (map vector
                (v/mult r (s 0))
                (v/mult g (s 1))
                (v/mult b (s 2)))))
;; => ([0.4124585781796624 0.35757553616579035 0.1804374332363275]
;;     [0.21267395437388842 0.7151510723315807 0.072174973294531]
;;     [0.019333995852171656 0.11919184538859676 0.9503038150446582])


(m/unuse-primitive-operators)
