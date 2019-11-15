(ns clojure2d.extra.glitch-example
  (:require [clojure2d.extra.glitch :refer :all]
            [metadoc.examples :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.core :refer :all]
            [clojure2d.color :as c]
            [fastmath.random :as r]))

(r/set-seed! r/default-rng 42)

(def samurai (p/load-pixels "docs/samurai.jpg"))

(defsnippet clojure2d.extra.glitch save-result "Save glitch"
  (let [n (str "images/glitch/" (first opts) ".jpg")]
    (save f (str "docs/" n))
    (str "../" n)))

;;

(add-examples slitscan-random-config
  (example-session "Usage"
    (slitscan-random-config)
    (slitscan-random-config 1 1)))

(add-examples slitscan
  (example-snippet "Random slitscan" save-result :image (p/filter-channels (slitscan) samurai))
  (example-snippet "Slitscan with parameters" save-result :image (p/filter-channels (slitscan {:x [{:wave :triangle, :freq 4, :amp 0.25, :phase 0.8}], :y [{:wave :sin, :freq 2, :amp 0.5, :phase 0.5}]}) samurai)))

;;

(add-examples shift-channels-random-config
  (example-session "Usage"
    (shift-channels-random-config)
    (shift-channels-random-config 100)))

(add-examples shift-channels
  (example-snippet "Shift red and blue channels" save-result :image
    (p/filter-channels (shift-channels) nil (shift-channels) nil samurai))
  (example-snippet "Shift hue and saturation channels" save-result :image
    (let [filt (shift-channels {:x-shift -0.2
                                :y-shift 1.0})]
      (->> samurai
           (p/filter-colors c/to-HSV*)
           (p/filter-channels filt filt nil nil)
           (p/filter-colors c/from-HSV*)))))
;;

(add-examples slitscan2-random-config
  (example-session "Usage"
    (slitscan2-random-config)
    (slitscan2-random-config 3.1234)
    (slitscan2-random-config 3.1234 0)))

(add-examples slitscan2
  (example-snippet "Slitscan with random config" save-result :image
    (p/filter-channels (slitscan2) samurai))
  (example-snippet "Provided config" save-result :image
    (let [conf {:fields {:type :operation, :name :comp, :var1 {:type :variation, :name :pressure-wave, :amount 1.0, :config {:x-freq -0.27, :y-freq 5.92}}, :var2 {:type :variation, :name :besselj, :amount 1.0, :config {}}, :amount 1.0}, :r 4.0}]
      (p/filter-channels (slitscan2 conf) samurai)))
  (example-snippet "Provided config with different range value for each channel" save-result :image
    (let [conf {:fields {:type :operation, :name :comp, :var1 {:type :variation, :name :perlin2, :amount 1.0, :config {:seed 112, :scale -1.13, :octaves 4}}, :var2 {:type :variation, :name :exponential, :amount 1.0, :config {}}, :amount 1.0}, :r 2.0}]
      (p/filter-channels (slitscan2 (assoc conf :r 1.9))
                         (slitscan2 (assoc conf :r 2.0))
                         (slitscan2 (assoc conf :r 2.1)) nil samurai))))

;;

(add-examples fold-random-config
  (example-session "Usage"
    (fold-random-config)
    (fold-random-config 3.1234)
    (fold-random-config 3.1234 0)))

(add-examples fold
  (example-snippet "Fold with random config" save-result :image
    (p/filter-channels (fold) samurai))
  (example-snippet "Provided config" save-result :image
    (let [conf {:fields {:type :operation, :name :comp, :var1 {:type :variation, :name :pressure-wave, :amount 1.0, :config {:x-freq -0.27, :y-freq 5.92}}, :var2 {:type :variation, :name :besselj, :amount 1.0, :config {}}, :amount 1.0}, :r 4.0}]
      (p/filter-channels (fold conf) samurai)))
  (example-snippet "Provided config with different range value for each channel" save-result :image
    (let [conf {:fields {:type :operation, :name :comp, :var1 {:type :variation, :name :perlin2, :amount 1.0, :config {:seed 112, :scale -1.13, :octaves 4}}, :var2 {:type :variation, :name :exponential, :amount 1.0, :config {}}, :amount 1.0}, :r 2.0}]
      (p/filter-channels (fold (assoc conf :r 1.9))
                         (fold (assoc conf :r 2.0))
                         (fold (assoc conf :r 2.1)) nil samurai))))


;;

(add-examples mirror-types
  (example "List of mirror function keys" (sort (keys mirror-types))))

(add-examples mirror-random-config
  (example-session "Usage"
    (mirror-random-config)
    (mirror-random-config)
    (mirror-random-config)))

(add-examples mirror
  (example-snippet "Random mirror" save-result :image (p/filter-channels (mirror) samurai))
  (example-snippet "Mirror R(ight) to left" save-result :image (p/filter-channels (mirror :R) samurai))
  (example-snippet "Mirror each channel separately" save-result :image
    (p/filter-channels (mirror :L) (mirror :R) (mirror :U) nil samurai)))

;;

(add-examples pix2line-random-config
  (example (pix2line-random-config)))

(add-examples pix2line
  (example-snippet "Random pix2line" save-result :image (p/filter-channels (pix2line) samurai))
  (example-snippet "With configuration, slightly modified for each channel" save-result :image (let [conf {:nx 9, :ny 3, :scale 4.2, :tolerance 37, :nseed -1, :whole true, :shiftx 0.66, :shifty 0.46}]
                                                                                                 (p/filter-channels (pix2line conf) (pix2line (assoc conf :scale 1)) (pix2line (assoc conf :scale 10 :tolerance 100)) nil samurai))))

;;

(add-examples blend-machine-random-config
  (example (blend-machine-random-config)))

(add-examples blend-machine
  (example-snippet "Usage" save-result :image
    (let [blend-conf {:blend-ch3 :hardmix, :out-cs :Cubehelix, :blend-ch2 :mlinearburn, :switch? true, :out-to? true, :in1-cs :LUV, :in1-to? true, :in2-cs :XYZ, :blend-ch1 :mdodge, :in2-to? true}
          p2l-conf {:nx 9, :ny 3, :scale 4.2, :tolerance 37, :nseed -1, :whole true, :shiftx 0.66, :shifty 0.46}
          p2 (p/filter-channels (pix2line p2l-conf) samurai)]
      (blend-machine blend-conf samurai p2))))

;;



(comment save-result  [] "aaa")
