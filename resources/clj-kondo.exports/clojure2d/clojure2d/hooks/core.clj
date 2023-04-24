(ns hooks.core)

(defmacro with-oriented-canvas
  [_orientation [c canv] & body]
  `(clojure.core/let [~c ~canv] ~@body))

(defmacro with-oriented-canvas->
  [_orientation canv & body]
  `(clojure.core/-> ~canv ~@body))

(defmacro make-set-color-fn
  [_doc n f]
  `(clojure.core/def ~n ~f))
