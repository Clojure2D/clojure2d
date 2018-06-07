(ns clojure2d.extra.signal-example
  (:require [metadoc.examples :refer :all]
            [clojure2d.extra.signal :refer :all]
            [clojure2d.pixels :as p]))


(add-examples pixels->signal
  (example "Basic usage" (pixels->signal (p/load-pixels "docs/cockatoo.jpg")))
  (example "Custom configuration" (pixels->signal (p/load-pixels "docs/cockatoo.jpg") {:planar? false
                                                                                       :signed? true
                                                                                       :bits 24
                                                                                       :coding :ulaw
                                                                                       :channels [2 1]})))

(add-examples signal->pixels
  (example "Basic usage" (let [p (p/load-pixels "docs/cockatoo.jpg")
                               target (p/clone-pixels p)]
                           (signal->pixels target (pixels->signal p))))
  (example "Custom configuration" (let [p (p/load-pixels "docs/cockatoo.jpg")
                                        target (p/clone-pixels p)
                                        conf-in {:planar? false
                                                 :signed? true
                                                 :bits 24
                                                 :coding :alaw
                                                 :channels [2 1]}
                                        conf-out (merge conf-in {:coding :alaw-rev})]
                                    (signal->pixels target (pixels->signal p conf-in) conf-out))))

(add-examples compose-effects
  (example "Usage" (let [effect1 (effect :simple-lowpass)
                         effect2 (effect :simple-highpass {:cutoff 2000.0})
                         composed (compose-effects effect1 effect2)]
                     [(str composed) (composed 0.5)])))

((effect :simple-lowpass) 0.5)

(comment let [p (p/load-pixels "b18.jpg")
              s (pixels->signal p)
              e (effect :simple-lowpass)
              r (apply-effects e s)]
         (save (signal->pixels p r) "basdfasf.jpg"))


(comment clojure2d.core/save (let [p (p/load-pixels "docs/cockatoo.jpg")
                                   target (p/clone-pixels p)]
                               (signal->pixels target (pixels->signal p {:planar? false
                                                                         :bits 24
                                                                         :signed? true
                                                                         :coding :alaw
                                                                         :channels [2 1]}) {:planar? false
                                                                                            :signed? true
                                                                                            :bits 24
                                                                                            :coding :alaw-rev
                                                                                            :channels [2 1]})) "basdfasf.jpg")
