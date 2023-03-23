(ns clojure2d.core)

(defmacro with-oriented-canvas
  [_orientation [c canv] & body]
  `(clojure.core/let [~c ~canv]
     ~@body))

(defmacro with-oriented-canvas->
  [_orientation canv & body]
  `(clojure.core/-> ~canv
     ~@body))
