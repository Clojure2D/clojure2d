(ns clojure2d.extra.signal
  "Image pixels as signal
  
  ### Pixels as Signal

  `Pixels` (`Image`) can be treated as signal. Conversion to signal is based on strategies of converting image to RAW and then converting to audio. It includes channel data layout and packing into integer, encoding, endianess, etc.

  To convert `Pixels` to signal use [[pixels->signal]] function. To convert back use [[signal->pixels]]. [[signal->pixels]] requires target `Pixels` object to store result of conversion. Target is mutated then.

  To filter `Pixels` directly (without explicit conversion to and from Signals) you can use [[filter-channels]] with [[effects-filter]]."
  {:metadoc/categories {:eff "Effects"                        
                        :sig "Signal"}}
  (:require [fastmath.core :as m]
            [clojure2d.pixels :as p]
            [fastmath.signal :as s])
  (:import [clojure2d.pixels Pixels]
           [clojure2d.java.signal Converter]))

(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn- coding-value
  "Encode `coding` as Converter class value."
  [coding]
  (condp = coding
    :alaw Converter/CODING_ALAW
    :ulaw Converter/CODING_ULAW
    :alaw-rev Converter/CODING_ALAW_REV
    :ulaw-rev Converter/CODING_ULAW_REV
    0))

(defn pixels->signal
  "Convert `Pixels` to `Signal` for given specifications.

  Specification for conversion is based on Audacity importing options and image to RAW conversions practices. Spec contains:

  * `planar?` - layout of channels data, planar (true, default) or interleaved (false)
  * `signed?` - channel data are encoded as signed (true, default) or unsigned (false) integer.
  * `little-endian?` - for 16 and 24 bits pack channel data with little-endian order (true, default) or big-endian (false)
  * `bits` - pack channel data in 8 (default), 16 or 24 bits.
  * `channels` - list of channels to convert to signal, default `[0 1 2]` (for all you can use `:all` keyword).
  * `coding` - encode signal with one of the encoders: `:alaw`, `:alaw-rev`, `:ulaw`, `:ulaw-rev` or `:none` (default).

  Pixels can be restored from Signal with [[signal->pixels]] function."
  {:metadoc/categories #{:sig}}
  ([^Pixels p {:keys [planar? signed? little-endian? ^int bits channels coding]
               :or {planar? true little-endian? true bits 8 signed? false channels [0 1 2] coding :none}}]
   (let [channels (int-array (if (= :all channels) [0 1 2 3] channels))]
     (Converter/toSignal
      (.p p) channels
      (dec (m/>> bits 3))
      little-endian? signed? planar?
      (coding-value coding))))
  ([p]
   (pixels->signal p {})))

(defn signal->pixels
  "Convert `Signal` to `Pixels` storing result into `target` (mutating it!).

  Specification is the same as in [[pixels-signal]] functions."
  {:metadoc/categories #{:sig}}
  ([sig ^Pixels target {:keys [planar? signed? little-endian? ^int bits channels coding]
                        :or {planar? true little-endian? true bits 8 signed? false channels [0 1 2] coding :none}}]
   (let [channels (int-array (if (= :all channels) [0 1 2 3] channels))]
     (Converter/fromSignal
      sig
      (.p target)
      channels
      (dec (m/>> bits 3))
      little-endian? signed? planar?
      (coding-value coding))
     target))
  ([sig target]
   (signal->pixels sig target {})))

(defn- process-pixels
  "Process pixels as signal based on config for given pixels p. Store result in given target t."
  [p t effects config config-back reset]
  (signal->pixels (s/apply-effects-raw (pixels->signal p config) effects reset) t config-back))

(defn effects-filter
  "Creates filter to process `Pixels` as `Signal` using [[filter-channels]].

  Provide configurations to properly convert Pixels to Signal and back.
  Optionally set `reset` value (reinit effects' state after `reset` samples).

  Filter operates on one channel at time and is defined to be used with [[filter-channels]].
  If you want to operate on `Pixels` directly, use [[apply-effects-to-pixels]]."
  {:metadoc/categories #{:eff}}
  ([effects config config-back reset]
   (fn [ch target p]
     (let [c {:channels [ch]}]
       (process-pixels p target effects (merge config c) (merge config-back c) reset))))
  ([effects]
   (effects-filter effects {} {} 0))
  ([effects reset]
   (effects-filter effects {} {} reset))
  ([effects config config-back]
   (effects-filter effects config config-back 0)))

(defn apply-effects-to-pixels
  "Apply effects directly to `Pixels`.

  Provide configurations to properly convert Pixels to Signal and back.
  Optionally set `reset` value (reinit effects' state after `reset` samples).
    
  If you prefer operating through [[filter-channels]] use [[effect-filter]]."
  {:metadoc/categories #{:eff}}
  ([effects config config-back reset pixels]
   (process-pixels pixels (p/clone-pixels pixels) effects config config-back reset))
  ([effects pixels]
   (apply-effects-to-pixels effects {} {} 0 pixels))
  ([effects reset pixels]
   (apply-effects-to-pixels effects {} {} reset pixels))
  ([effects config config-back pixels]
   (apply-effects-to-pixels effects config config-back 0 pixels)))

