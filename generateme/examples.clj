(ns examples
  (:require [clojure2d.math :as m]
            [clojure.test :refer :all]))

(defonce registry-ref (atom {}))

(defn parse-ns [k]
  (try (symbol (namespace k))
       (catch Exception _)))

(defn parse-val [v]
  (cond
    (symbol? v) (symbol (name v))
    (keyword? v) (keyword (name v))
    :else v))

(defn parse-keys [k]
  [(or (parse-ns k) (symbol (str *ns*)))
   (parse-val k)])

(defn parse-example [args]
  (let [{:keys [meta body]} (s/conform ::args args)
        _ (when (= meta :clojure.spec/invalid)
            (throw (Exception. (str "Invalid args: " (pr-str args)))))
        [meta-type meta-val] meta
        opts (case meta-type
               :doc {:doc meta-val}
               :opts meta-val
               {})
        body (if (> (count body) 1)
               (apply list 'do body)
               (first body))]
    (assoc opts :body body)))

(defn defexample*
  "Like defexample, but a function instead of a macro"
  [k & args]
  (let [[ns-sym k] (parse-keys k)
        example (parse-example args)]
    (swap! registry-ref assoc-in [ns-sym k] [example])
    nil))

(defn defexamples*
  "Like defexamples, but a function instead of a macro"
  [k & examples]
  (let [[ns-sym k] (parse-keys k)]
    (swap! registry-ref assoc-in [ns-sym k]
           (mapv parse-example examples))
    nil))

(defmacro defexample
  "Defines one example code block for a symbol or an arbitrary
  piece of Clojure data. If `k` is not a namespace-qualified symbol or
  keyword, it will be associated with the current namespace."
  [k & args]
  (apply defexample* k args))

(defmacro defexamples
  "Defines multiple example code blocks for a symbol or an arbitrary
  piece of Clojure data. If `k` is not a namespace-qualified symbol or
  keyword, it will be associated with the current namespace."
  [k & examples]
  (apply defexamples* k examples))

(defexamples defexample
  ["Define an example of a function in another namespace"
   (defexample clojure.core/+
     "Add two numbers together"
     (+ 1 1))]
  ["Define an example of a function in the current namespace"
   (defexample parse-ns
     "Get the namespace from a symbol"
     (parse-ns 'my.namespace/asdf))]
  ["Define an example of a function with an assertion for testing"
   (defexample parse-ns
     {:doc "Get the namespace from a symbol"
      :ret (fn [n] (= n 'my.namespace))}
     (parse-ns 'my.namespace/asdf))])

(defexample defexamples
  "Define multiple examples of the `conj` function"
  (defexamples clojure.core/conj
    ["Add a name to a vector"
     (conj ["Alice" "Bob"] "Charlie")]
    ["Add a number to a list"
     (conj '(2 3) 1)]
    [{:doc "Add a key-val pair to a hash map"
      :ret (fn [m] (= m {:name "Alice" :age 30}))}
     (conj {:name "Alice"} [:age 30])]))


(defmacro eee
  [desc & r]
  `{:desc ~desc
    :examples (list ~@(for [x# r]
                        [(str x#) `(fn [] ~x#)]))})

(defn ^{:ex (eee "dddd" (let [c m/PI] (ffff c)) m/PI)} ffff
  "ssss" 
  [a]
  (+ a 2))


(def ^{:doc "adsfasfdfe"
       :ex (eee "dfsfsfsf" aaaaaa)} aaaaaa 2123.2)


((second (second (:examples (:ex (meta #'ffff))))))

(meta (second (first (ns-interns *ns*))))

(meta (with-meta 'clojure2d.math/LOG2E {:ex (eee "dddsaere" m/LOG2E)}))

(type eee)

(prn *ns*)

(eee "dddsaere" m/LOG2E)

(clojure.repl/doc aaaaaa)

(is (= 3 (+ 2 1)))

(with-test
  (defn abc [] "abc")
  (is (= "abc" (abc)))
  (is (not= "abc" (abc))))

((:test (meta #'abc)))

(deftest blahtest
  (is (= 33 (+ 2 1))))

((:test (meta #'blahtest)))

(test-var #'blahtest)


(run-tests)
