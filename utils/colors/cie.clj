(ns colors.cie
  (:require [clojure.string :as str]
            [clojure.data.json :as json]
            [fastmath.vector :as v]
            [clojure2d.color.whitepoints :as wp])
  (:import [fastmath.vector Vec2 Vec3]))

;; Illuminants

;; https://github.com/colour-science/colour/blob/develop/colour/colorimetry/datasets/illuminants/sds.py
;; colour.colorimetry.datasets.illuminants.sds.DATA_ILLUMINANTS_CIE
;; colour.colorimetry.datasets.illuminants.sds.DATA_ILLUMINANTS_ISO
;; saved as JSON

;; import json
;; with open('data/data.json', 'w', encoding='utf-8') as f:
;;     json.dump(data, f, ensure_ascii=False, indent=4)

(defn process-pair [[l v]] [(parse-long l) v])

(defn process-pairs
  [pairs]
  (let [[l v] (->> pairs
                   (map process-pair)
                   (sort-by first)
                   (apply map vector))]
    {:lambda l :value v :range [(first l) (last l)]}))

(def illuminants-keys-mapping {:ISO-7589-Photoflood :ISO-PF
                             :ISO-7589-Photographic-Daylight :ISO-PD
                             :ISO-7589-Sensitometric-Daylight :ISO-SD
                             :ISO-7589-Sensitometric-Photoflood :ISO-SPF
                             :ISO-7589-Sensitometric-Printer :ISO-SP
                             :ISO-7589-Sensitometric-Studio-Tungsten :ISO-SST
                             :ISO-7589-Studio-Tungsten :ISO-ST})

(defn process-keys
  [k]
  (let [v (keyword (str/replace k #"[\.\s]" "-"))]
    (get illuminants-keys-mapping v v)))

(def illuminants
  (let [ills (merge (json/read-str (slurp "data/illuminants_cie.json"))
                    (json/read-str (slurp "data/illuminants_iso.json")))]
    (-> ills
        (update-keys process-keys)
        (update-vals process-pairs))))

(spit "resources/color/illuminants.edn" (pr-str illuminants))

;; Colour Matching Functions

;; https://github.com/colour-science/colour/blob/develop/colour/colorimetry/datasets/cmfs.py#L1374
;; colour.colorimetry.datasets.cmfs.DATA_CMFS_STANDARD_OBSERVER

(defn process-triplet [[l [x y z]]] [(parse-long l) x y z])

(defn process-triplets
  [pairs]
  (let [[l x y z] (->> pairs
                       (map process-triplet)
                       (sort-by first)
                       (apply map vector))]
    {:lambda l :x x :y y :z z :range [(first l) (last l)]}))

(def cmf-keys-mapping {:CIE-1931-2-Degree-Standard-Observer :CIE-2
                     :CIE-1964-10-Degree-Standard-Observer :CIE-10
                     :CIE-2015-10-Degree-Standard-Observer :CIE-2015-10
                     :CIE-2015-2-Degree-Standard-Observer :CIE-2015-2})

(def cmfs
  (let [cmfs (json/read-str (slurp "data/cmfs_standard_observer.json"))]
    (-> cmfs
        (update-keys (comp cmf-keys-mapping process-keys))
        (update-vals process-triplets))))

(spit "resources/color/cmfs_standard_observer.edn" (pr-str cmfs))

;;

;; https://github.com/colour-science/colour/blob/develop/colour/colorimetry/datasets/illuminants/chromaticity_coordinates.py

;; CIE 2 1931

(def CIE-2-A              (Vec2. 0.44758 0.40745))
(def CIE-2-B              (Vec2. 0.34842 0.35161))
(def CIE-2-C              (Vec2. 0.31006 0.31616))
(def CIE-2-D50            (Vec2. 0.34567 0.35851))
(def CIE-2-D55            (Vec2. 0.33243 0.34744))
(def CIE-2-D60            (Vec2. 0.321616709705268 0.337619916550817))
(def CIE-2-D65            (Vec2. 0.312727 0.329023)) ;; better precision
(def CIE-2-D75            (Vec2. 0.29903 0.31488))
(def CIE-2-E              (Vec2. 0.3333333333333333 0.3333333333333333))
(def CIE-2-FL1            (Vec2. 0.31310 0.33710))
(def CIE-2-FL2            (Vec2. 0.37210 0.37510))
(def CIE-2-FL3            (Vec2. 0.40910 0.39410))
(def CIE-2-FL4            (Vec2. 0.44020 0.40310))
(def CIE-2-FL5            (Vec2. 0.31380 0.34520))
(def CIE-2-FL6            (Vec2. 0.37790 0.38820))
(def CIE-2-FL7            (Vec2. 0.31290 0.32920))
(def CIE-2-FL8            (Vec2. 0.34580 0.35860))
(def CIE-2-FL9            (Vec2. 0.37410 0.37270))
(def CIE-2-FL10           (Vec2. 0.34580 0.35880))
(def CIE-2-FL11           (Vec2. 0.38050 0.37690))
(def CIE-2-FL12           (Vec2. 0.43700 0.40420))
(def CIE-2-FL3-1          (Vec2. 0.44070 0.40330))
(def CIE-2-FL3-2          (Vec2. 0.38080 0.37340))
(def CIE-2-FL3-3          (Vec2. 0.31530 0.34390))
(def CIE-2-FL3-4          (Vec2. 0.44290 0.40430))
(def CIE-2-FL3-5          (Vec2. 0.37490 0.36720))
(def CIE-2-FL3-6          (Vec2. 0.34880 0.36000))
(def CIE-2-FL3-7          (Vec2. 0.43840 0.40450))
(def CIE-2-FL3-8          (Vec2. 0.38200 0.38320))
(def CIE-2-FL3-9          (Vec2. 0.34990 0.35910))
(def CIE-2-FL3-10         (Vec2. 0.34550 0.35600))
(def CIE-2-FL3-11         (Vec2. 0.32450 0.34340))
(def CIE-2-FL3-12         (Vec2. 0.43770 0.40370))
(def CIE-2-FL3-13         (Vec2. 0.38300 0.37240))
(def CIE-2-FL3-14         (Vec2. 0.34470 0.36090))
(def CIE-2-FL3-15         (Vec2. 0.31270 0.32880))
(def CIE-2-HP1            (Vec2. 0.53300 0.4150))
(def CIE-2-HP2            (Vec2. 0.47780 0.41580))
(def CIE-2-HP3            (Vec2. 0.43020 0.40750))
(def CIE-2-HP4            (Vec2. 0.38120 0.37970))
(def CIE-2-HP5            (Vec2. 0.37760 0.37130))
(def CIE-2-LED-B1         (Vec2. 0.45600 0.40780))
(def CIE-2-LED-B2         (Vec2. 0.43570 0.40120))
(def CIE-2-LED-B3         (Vec2. 0.37560 0.37230))
(def CIE-2-LED-B4         (Vec2. 0.34220 0.35020))
(def CIE-2-LED-B5         (Vec2. 0.31180 0.32360))
(def CIE-2-LED-BH1        (Vec2. 0.44740 0.40660))
(def CIE-2-LED-RGB1       (Vec2. 0.45570 0.42110))
(def CIE-2-LED-V1         (Vec2. 0.45480 0.40440))
(def CIE-2-LED-V2         (Vec2. 0.37810 0.37750))
(def CIE-2-ID65           (Vec2. 0.310656625403120 0.330663091836953))
(def CIE-2-ID50           (Vec2. 0.343211370103531 0.360207541805137))
(def CIE-2-ACES           (Vec2. 0.32168, 0.33767))
(def CIE-2-BLACKMAGIC-WG  (Vec2. 0.3127170, 0.3290312)) ;; Blackmagic Wide Gamut
(def CIE-2-DCI-P3         (Vec2. 0.31400, 0.35100))
(def CIE-2-ICC-D50       (Vec2. 0.345702914918791, 0.358538596679933))
(def CIE-2-ISO-PD        (Vec2. 0.332039098470978, 0.347263885596614)) ;; Photographics Daylight
(def CIE-2-ISO-SD        (Vec2. 0.333818313227557, 0.353436231513603)) ;; Sensitometric Daylight
(def CIE-2-ISO-ST        (Vec2. 0.430944089109761, 0.403585442674295)) ;; Studio Tungsten
(def CIE-2-ISO-SST       (Vec2. 0.431418223648390, 0.407471441950342)) ;; Sensitometric Studio Tungsten
(def CIE-2-ISO-PF        (Vec2. 0.411146015714843, 0.393719378241161)) ;; Photoflood
(def CIE-2-ISO-SPF       (Vec2. 0.412024776908998, 0.398177410548532)) ;; Sensitometric Photoflood
(def CIE-2-ISO-SP        (Vec2. 0.412087967973680, 0.421104984758526)) ;; Sensitometric Printer
(def CIE-2-PLASA         (Vec2. 0.4254, 0.4044))

;; CIE 10 1964
(def CIE-10-A        (Vec2. 0.45117 0.40594))
(def CIE-10-B        (Vec2. 0.34980 0.35270))
(def CIE-10-C        (Vec2. 0.31039 0.31905))
(def CIE-10-D50      (Vec2. 0.34773 0.35952))
(def CIE-10-D55      (Vec2. 0.33412 0.34877))
(def CIE-10-D60      (Vec2. 0.322986926715820 0.339275732345997))
(def CIE-10-D65      (Vec2. 0.31381 0.33098))
(def CIE-10-D75      (Vec2. 0.29968 0.31740))
(def CIE-10-E        (Vec2. 0.3333333333333333 0.3333333333333333))
(def CIE-10-FL1      (Vec2. 0.31811 0.33559))
(def CIE-10-FL2      (Vec2. 0.37925 0.36733))
(def CIE-10-FL3      (Vec2. 0.41761 0.38324))
(def CIE-10-FL4      (Vec2. 0.44920 0.39074))
(def CIE-10-FL5      (Vec2. 0.31975 0.34246))
(def CIE-10-FL6      (Vec2. 0.38660 0.37847))
(def CIE-10-FL7      (Vec2. 0.31569 0.32960))
(def CIE-10-FL8      (Vec2. 0.34902 0.35939))
(def CIE-10-FL9      (Vec2. 0.37829 0.37045))
(def CIE-10-FL10     (Vec2. 0.35090 0.35444))
(def CIE-10-FL11     (Vec2. 0.38541 0.37123))
(def CIE-10-FL12     (Vec2. 0.44256 0.39717))
(def CIE-10-FL3-1    (Vec2. 0.449830684010003 0.390231404321266))
(def CIE-10-FL3-2    (Vec2. 0.386924116672933 0.365756034732821))
(def CIE-10-FL3-3    (Vec2. 0.321176986855865 0.340501092654981))
(def CIE-10-FL3-4    (Vec2. 0.448121275113995 0.397077112142482))
(def CIE-10-FL3-5    (Vec2. 0.377814166608895 0.366625766963060))
(def CIE-10-FL3-6    (Vec2. 0.351976478983504 0.361094432889677))
(def CIE-10-FL3-7    (Vec2. 0.444309208810922 0.396791387314871))
(def CIE-10-FL3-8    (Vec2. 0.387588931999771 0.376305569410173))
(def CIE-10-FL3-9    (Vec2. 0.354688990710449 0.353445033593383))
(def CIE-10-FL3-10   (Vec2. 0.349344792334400 0.354984421140869))
(def CIE-10-FL3-11   (Vec2. 0.329267975695120 0.338865386643537))
(def CIE-10-FL3-12   (Vec2. 0.442252080438001 0.401220551071252))
(def CIE-10-FL3-13   (Vec2. 0.386275268780817 0.374283190950586))
(def CIE-10-FL3-14   (Vec2. 0.347255078638291 0.366808242504180))
(def CIE-10-FL3-15   (Vec2. 0.314613997909246 0.333377149377113))
(def CIE-10-HP1      (Vec2. 0.543334600247307 0.405289298480431))
(def CIE-10-HP2      (Vec2. 0.482647330648721 0.410815644179685))
(def CIE-10-HP3      (Vec2. 0.435560034503954 0.398801084399711))
(def CIE-10-HP4      (Vec2. 0.385193641123543 0.368275479241015))
(def CIE-10-HP5      (Vec2. 0.380316415606638 0.366617114797851))
(def CIE-10-LED-B1   (Vec2. 0.462504966271043 0.403041801546906))
(def CIE-10-LED-B2   (Vec2. 0.442119475258745 0.396633702892576))
(def CIE-10-LED-B3   (Vec2. 0.380851979328052 0.368518548904765))
(def CIE-10-LED-B4   (Vec2. 0.348371362473402 0.345065503264192))
(def CIE-10-LED-B5   (Vec2. 0.316916877024753 0.322060276350364))
(def CIE-10-LED-BH1  (Vec2. 0.452772610754910 0.400032462750000))
(def CIE-10-LED-RGB1 (Vec2. 0.457036370583652 0.425381348780888))
(def CIE-10-LED-V1   (Vec2. 0.453602699414564 0.398199587905174))
(def CIE-10-LED-V2   (Vec2. 0.377728483834020 0.374512315539769))
(def CIE-10-ID65     (Vec2. 0.312074043269908 0.332660121024630))
(def CIE-10-ID50     (Vec2. 0.345621427535976 0.361228962209198))
(def CIE-10-ISO-PD        (Vec2. 0.333716908394534, 0.348592494683065)) ;; Photographics Daylight
(def CIE-10-ISO-SD        (Vec2. 0.336125906007630, 0.354997062476417)) ;; Sensitometric Daylight
(def CIE-10-ISO-ST        (Vec2. 0.434575926493196, 0.402219691745325)) ;; Studio Tungsten
(def CIE-10-ISO-SST       (Vec2. 0.435607674215215, 0.406129244796761)) ;; Sensitometric Studio Tungsten
(def CIE-10-ISO-PF         (Vec2. 0.414144647169611, 0.392458587686395)) ;; Photoflood
(def CIE-10-ISO-SPF        (Vec2. 0.415625819190627, 0.397002292994179)) ;; Sensitometric Photoflood
(def CIE-10-ISO-SP       (Vec2. 0.418841052206998, 0.418695130974955)) ;; Sensitometric Printer

(def chromacity
  {:CIE-2 {:A              CIE-2-A
           :B              CIE-2-B     
           :C              CIE-2-C     
           :D50            CIE-2-D50   
           :D55            CIE-2-D55   
           :D60            CIE-2-D60   
           :D65            CIE-2-D65   
           :D75            CIE-2-D75   
           :E              CIE-2-E     
           :FL1            CIE-2-FL1   
           :FL2            CIE-2-FL2   
           :FL3            CIE-2-FL3   
           :FL4            CIE-2-FL4   
           :FL5            CIE-2-FL5   
           :FL6            CIE-2-FL6   
           :FL7            CIE-2-FL7   
           :FL8            CIE-2-FL8   
           :FL9            CIE-2-FL9   
           :FL10           CIE-2-FL10  
           :FL11           CIE-2-FL11  
           :FL12           CIE-2-FL12  
           :FL3-1          CIE-2-FL3-1 
           :FL3-2          CIE-2-FL3-2 
           :FL3-3          CIE-2-FL3-3 
           :FL3-4          CIE-2-FL3-4 
           :FL3-5          CIE-2-FL3-5 
           :FL3-6          CIE-2-FL3-6 
           :FL3-7          CIE-2-FL3-7 
           :FL3-8          CIE-2-FL3-8 
           :FL3-9          CIE-2-FL3-9 
           :FL3-10         CIE-2-FL3-10
           :FL3-11         CIE-2-FL3-11
           :FL3-12         CIE-2-FL3-12
           :FL3-13         CIE-2-FL3-13
           :FL3-14         CIE-2-FL3-14
           :FL3-15         CIE-2-FL3-15
           :HP1            CIE-2-HP1   
           :HP2            CIE-2-HP2   
           :HP3            CIE-2-HP3   
           :HP4            CIE-2-HP4   
           :HP5            CIE-2-HP5   
           :LED-B1         CIE-2-LED-B1
           :LED-B2         CIE-2-LED-B2
           :LED-B3         CIE-2-LED-B3
           :LED-B4         CIE-2-LED-B4
           :LED-B5         CIE-2-LED-B5
           :LED-BH1        CIE-2-LED-BH1      
           :LED-RGB1       CIE-2-LED-RGB1     
           :LED-V1         CIE-2-LED-V1       
           :LED-V2         CIE-2-LED-V2       
           :ID65           CIE-2-ID65         
           :ID50           CIE-2-ID50         
           :ACES           CIE-2-ACES         
           :BLACKMAGIC-WG  CIE-2-BLACKMAGIC-WG ;; Blackmagic Wide Gamut
           :DCI-P3         CIE-2-DCI-P3       
           :ICC-D50        CIE-2-ICC-D50      
           :ISO-PD         CIE-2-ISO-PD        ;; Photographics Daylight
           :ISO-SD         CIE-2-ISO-SD        ;; Sensitometric Daylight
           :ISO-ST         CIE-2-ISO-ST        ;; Studio Tungsten
           :ISO-SST        CIE-2-ISO-SST       ;; Sensitometric Studio Tungsten
           :ISO-PF         CIE-2-ISO-PF        ;; Photoflood
           :ISO-SPF        CIE-2-ISO-SPF       ;; Sensitometric Photoflood
           :ISO-SP         CIE-2-ISO-SP        ;; Sensitometric Printer
           :PLASA          CIE-2-PLASA}
   :CIE-10 {:A      CIE-10-A     
            :B      CIE-10-B     
            :C      CIE-10-C     
            :D50    CIE-10-D50   
            :D55    CIE-10-D55   
            :D60    CIE-10-D60   
            :D65    CIE-10-D65   
            :D75    CIE-10-D75   
            :E      CIE-10-E     
            :FL1    CIE-10-FL1   
            :FL2    CIE-10-FL2   
            :FL3    CIE-10-FL3   
            :FL4    CIE-10-FL4   
            :FL5    CIE-10-FL5   
            :FL6    CIE-10-FL6   
            :FL7    CIE-10-FL7   
            :FL8    CIE-10-FL8   
            :FL9    CIE-10-FL9   
            :FL10   CIE-10-FL10  
            :FL11   CIE-10-FL11  
            :FL12   CIE-10-FL12  
            :FL3-1  CIE-10-FL3-1 
            :FL3-2  CIE-10-FL3-2 
            :FL3-3  CIE-10-FL3-3 
            :FL3-4  CIE-10-FL3-4 
            :FL3-5  CIE-10-FL3-5 
            :FL3-6  CIE-10-FL3-6 
            :FL3-7  CIE-10-FL3-7 
            :FL3-8  CIE-10-FL3-8 
            :FL3-9  CIE-10-FL3-9 
            :FL3-10 CIE-10-FL3-10
            :FL3-11 CIE-10-FL3-11
            :FL3-12 CIE-10-FL3-12
            :FL3-13 CIE-10-FL3-13
            :FL3-14 CIE-10-FL3-14
            :FL3-15 CIE-10-FL3-15
            :HP1    CIE-10-HP1   
            :HP2    CIE-10-HP2   
            :HP3    CIE-10-HP3   
            :HP4    CIE-10-HP4   
            :HP5    CIE-10-HP5   
            :LED-B1 CIE-10-LED-B1
            :LED-B2 CIE-10-LED-B2
            :LED-B3 CIE-10-LED-B3
            :LED-B4 CIE-10-LED-B4
            :LED-B5 CIE-10-LED-B5
            :LED-BH1  CIE-10-LED-BH1 
            :LED-RGB1 CIE-10-LED-RGB1
            :LED-V1   CIE-10-LED-V1  
            :LED-V2   CIE-10-LED-V2  
            :ID65     CIE-10-ID65    
            :ID50     CIE-10-ID50    
            :ISO-PD   CIE-10-ISO-PD
            :ISO-SD   CIE-10-ISO-SD
            :ISO-ST   CIE-10-ISO-ST
            :ISO-SST  CIE-10-ISO-SST
            :ISO-PF   CIE-10-ISO-PF
            :ISO-SPF  CIE-10-ISO-SPF
            :ISO-SP   CIE-10-ISO-SP}})

(def white {:range [0 999]
          :lambda (range 1000)
          :value (repeat 1000 1.0)})

;; from cmfs and spectrum
(def whitepoints (into {} (for [os (keys cmfs)
                              is (keys illuminants)
                              :let [k (keyword (str (name os) "-" (name is) "*"))
                                    ->xyz (wp/->spectrum-to-XYZ1 os is {:interpolation :linear
                                                                        :extrapolation
                                                                        (if (#{:E} is) :constant :trim)})
                                    ^Vec3 XZ (->xyz white)
                                    ^Vec2 xy (wp/XZ->xy (v/vec2 (.x XZ) (.z XZ)))]]
                          [k [(.x XZ) (.z XZ) (.x xy) (.y xy)]])))

;; from table
(def whitepoints-2 (into {} (for [os (keys chromacity)
                                is (keys (chromacity os))
                                :let [k (keyword (str (name os) "-" (name is)))
                                      ^Vec2 xy (get-in chromacity [os is])
                                      ^Vec2 XZ (wp/xy->XZ xy)]]
                            [k [(.x XZ) (.y XZ) (.x xy) (.y xy)]])))

(spit "resources/color/whitepoints.edn" (prn-str (merge whitepoints whitepoints-2)))


#_(def tst (wp/spectrum (range 340 831 5)
                      [0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0641,
                       0.0650,
                       0.0654,
                       0.0652,
                       0.0645,
                       0.0629,
                       0.0605,
                       0.0581,
                       0.0562,
                       0.0551,
                       0.0543,
                       0.0539,
                       0.0537,
                       0.0538,
                       0.0541,
                       0.0547,
                       0.0559,
                       0.0578,
                       0.0603,
                       0.0629,
                       0.0651,
                       0.0667,
                       0.0680,
                       0.0691,
                       0.0705,
                       0.0720,
                       0.0736,
                       0.0753,
                       0.0772,
                       0.0791,
                       0.0809,
                       0.0833,
                       0.0870,
                       0.0924,
                       0.0990,
                       0.1061,
                       0.1128,
                       0.1190,
                       0.1251,
                       0.1308,
                       0.1360,
                       0.1403,
                       0.1439,
                       0.1473,
                       0.1511,
                       0.1550,
                       0.1590,
                       0.1634,
                       0.1688,
                       0.1753,
                       0.1828,
                       0.1909,
                       0.1996,
                       0.2088,
                       0.2187,
                       0.2291,
                       0.2397,
                       0.2505,
                       0.2618,
                       0.2733,
                       0.2852,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000,
                       0.0000]))
