(ns clojure2d.colors-gen-example
  (:require [hiccup.page :as p]
            [clojure2d.color :as c]
            [fastmath.core :as m]
            [clojure.string :as str]))

(defn emit-css-gradient
  [lst]
  (str "background-image: linear-gradient(90deg,"
       (str/join "," lst)
       ");"))

(def to-percent (comp #(str % "%") double #(/ % 1000) int #(* % 100000.0)))

(defn palette->css-gradient
  [p]
  (let [cp (count p)        
        steps (mapcat (fn [c [g1 g2]]
                        [(str c " " g1)
                         (str c " " g2)])
                      (map c/format-hex p)
                      (partition 2 1 (map (comp to-percent #(m/norm % 0 cp 0.0 1.0)) (range (inc cp)))))]
    (emit-css-gradient steps)))

(def pcts-doubles (map #(m/norm % 0 29 0.0 1.0) (range 30)))
(def pcts-strs (map to-percent pcts-doubles))
(def pcts (map vector pcts-doubles pcts-strs))

(defn gradient->css-gradient
  [f]
  (emit-css-gradient (for [[p ps] pcts]
                       (str (c/format-hex (f p)) " " ps))))

(defn gradient-div [emit-fn gradient-or-palette n]
  [:div {:class "gg gs"}
   [:div {:class "gb gs"}]
   [:div {:class "g gs"
          :style (emit-fn gradient-or-palette)}
    [:div {:id "name"} n]]])

(def palette->div (partial gradient-div palette->css-gradient))
(def gradient->div (partial gradient-div gradient->css-gradient))

(defn color-spans [c]
  (let [str-color (if (< (c/luma c) 128.0) "white" "black")
        n (c/format-hex c)]
    [:span.color {:style (str "background-color:" n ";color:" str-color ";")} (str "[" n "] " c)]))

(spit "docs/static/colors.html"
      (p/html5
       (p/include-css "static.css")
       (for [k (sort-by c/luma (c/named-colors-list))]
         [:pre.color
          [:div (color-spans k)]])
       [:div {:class "spacer"} "&nbsp;"]
       [:div
        [:span {:class "name"} [:a {:href "colors.html"} "COLORS"]]
        [:span {:class "name"} [:a {:href "palettes/index.html"} "PALETTES"]]
        [:span {:class "name"} [:a {:href "gradients/index.html"} "GRADIENTS"]]]
       [:div {:class "spacer"} "&nbsp;"]))

(defn generate-link-list
  [title lst]
  [:div {:class "list"}
   [:h3 title]
   (for [k lst]
     [:div {:class "namelink"}
      [:a {:href (str (or k "index") ".html")} (or k "MAIN")]])])

(defn generate-htmls
  [object-finder gradient-or-palette generator path]
  (let [namespaces (distinct (map #(if (keyword? %)
                                     (namespace %)
                                     "COLOURlovers") (object-finder)))
        link-list (generate-link-list "List of all groups." namespaces)]
    (doseq [group namespaces
            :let [ks (cond
                       (= group "COLOURlovers") (sort (object-finder #"^[0-9]+$"))
                       group (object-finder (re-pattern (str "^:" group "/")))
                       :else (remove number? (object-finder #"^((?!/).)*$")))]]
      (spit (str "docs/static/" path "/" (or group "index") ".html")
            (p/html5
             (p/include-css "../static.css")
             [:div {:class "content"}
              [:h2 group]
              [:div {:class "list"}
               (for [k ks
                     :let [g (gradient-or-palette k)]]
                 (generator g (str k)))]
              [:div {:class "spacer"} "&nbsp;"]
              link-list
              [:div {:class "spacer"} "&nbsp;"]
              [:div
               [:span {:class "name"} [:a {:href "../colors.html"} "COLORS"]]
               [:span {:class "name"} [:a {:href "../palettes/index.html"} "PALETTES"]]
               [:span {:class "name"} [:a {:href "../gradients/index.html"} "GRADIENTS"]]]
              [:div {:class "spacer"} "&nbsp;"]])))))

(generate-htmls c/find-gradient c/gradient gradient->div "gradients")

(generate-htmls c/find-palette c/palette palette->div "palettes")
