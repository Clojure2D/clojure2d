(ns hooks.whitepoints)

(defmacro gen-whitepoints
  [wp]
  `(declare ~@(for [o wp]
                `~(symbol (name o)))))
