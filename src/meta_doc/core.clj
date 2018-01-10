(ns meta-doc.core
  (:require [clojure.string :as s]
            ;; [clojure2d.core :refer [with-canvas]]
            ))

(def ^:dynamic *load-examples* true)
(def ^:dynamic *alter-doc* true)

(def ^:const ^String new-line (System/getProperty "line.separator"))

(def escape-map {(char 0xffff) "\\0xffff"})

(defn alter-info
  ""
  [tag f s v]
  (when-let [tag-val (tag (meta v))] 
    (let [doc (:doc (meta v))]
      (alter-meta! v assoc :doc (str doc new-line new-line (f tag-val s v))))))

(def alter-const-info (partial alter-info :const #(s/escape (str "Constant value `" %2 " = " (var-get %3) "`") escape-map)))
(def alter-tag-info (partial alter-info :tag (fn [t & _] (str "Type: " (if (fn? t) (last (s/split (str (type t)) #"\$")) t)))))

(defn alter-docs
  ""
  ([] (alter-docs *ns*))
  ([ns]
   (when *alter-doc*
     (doseq [[s v] (ns-publics ns)]
       (alter-const-info s v)
       (alter-tag-info s v)))))

;; (alter-docs)

;;
(comment

  (defmacro example
    "Create example as vector of desription, literal example and function returning result."
    ([description xample]
     `[~description ~(str xample) (fn [] ~xample)]))

  (defn make-examples
    "Creates vector of examples."
    [& examples]
    (vec examples))

  (defn eval-examples
    "Evaluate examples created by make-examples macro."
    [examples]
    (map #(let [[description example example-fn] %]
            [description example (example-fn)]) examples))

  (defmacro with-examples
    "Attach provided examples to given var definition."
    [definition & examples]
    (if *load-examples*
      `(doto ~definition (alter-meta! assoc :examples (make-examples ~@examples)))
      definition))

  (defmacro defexamples
    "Defines examples function with name description and list of unit examples"
    [name description & examples]
    (when *load-examples*
      `(def ~(vary-meta name assoc :examples (vec examples) :doc description)
         (fn [] (eval-examples (make-examples ~@examples))))))

  (defmacro set-examples
    "Adds examples to the existing var"
    [name & examples]
    (when *load-examples* 
      `(alter-meta! (var ~name) assoc :examples (conj (or (:examples (meta (var ~name))) []) ~@examples))))

  ;;

  (defn eval-vars-examples
    "Generate map with vars and its examples"
    [vars]
    (for [[ns vars] (group-by (comp :ns meta) vars)]
      {:ns ns
       :vars (map #(when-let [ex (:examples (meta %))]
                     {(:name (meta %)) (eval-examples ex)}) vars)}))

  (eval-vars-examples [(var my-first-examples) (var extest)])

;;;;;;;;;;

  (defexamples my-first-examples
    "Tddfsf"
    (example "blah" 1.0)
    (example "sferer" (let [a 1] a)))



  (with-examples
    (def ^:privatsdfsdf extest 123)
    (example "blah" (inc extest)))

  (my-first-examples)

  (set-examples extest
                (example "123" 3.0))


  (eval-examples (:examples (meta (var extest))))
  )
