(ns ennoia.app
  (:require [reagent.core :as reagent]
            [re-frame.core :as rf]
            [clojure.string :as str]))

(defn concept-map []
[:svg {:viewBox="0 0 300 100" :xmlns "http://www.w3.org/2000/svg"}
  [:circle {:cx "50" :cy "50" :r "40" :stroke "red" :fill "grey"}]
  [:circle {:cx "150" :cy "50" :r "4" :stroke "red" :fill "grey"}]

  [:svg {:viewBox "0 0 10 10" :x "200" :width "100"}
    [:circle {:cx "5" :cy "5" :r "4" :stroke "red" :fill "grey"}]
    ]]
)

(defn ui []
 [:div
 [:p 
 ;[:button {:class "mdl-button mdl-js-button mdl-button--raised mdl-button--colored"}
 ;  "Button"]]

  [:div [:a {:class ["waves-effect waves-light btn"]}
             [:i {:class ["material-icons left"]} "cloud"]
             "button"]]]
 [concept-map]]
)

(defn ^:export run
  []
  (rf/dispatch-sync [:initialize])     ;; puts a value into application state
  (reagent/render [ui]              ;; mount the application's ui into '<div id="app" />'
                  (js/document.getElementById "app")))
