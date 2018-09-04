(ns ennoia.app
  (:require [reagent.core :as reagent]
            [re-frame.core :as rf]
            [clojure.string :as str]
            [ennoia.localization :as loc :refer [tr]]
            [ennoia.conceptmap :as cm]
            ))

; View Functions
(defn concept-map []
 (cm/cm->svg @(rf/subscribe [:current-map]))
)

(defn ui []
 [:div
     [:nav
        [:div {:class ["nav-wrapper"]}
            [:a {:href "#" :class "brand-logo"} (tr [:chrome/title])]
            ]
     ]

     [concept-map]]
)

; Event handlers

(rf/reg-event-db 
 :initialize
 (fn [_ _]
  (let [blank (cm/create-conceptmap)]
  { :maps { (:id blank) blank } :current-map-id (:id blank) }
 )))

(rf/reg-sub
  :current-map
  (fn [db _]
   (let [id (:current-map-id db)]
    (get-in db [:maps id])
    )))

(defn ^:export run
  []
  (rf/dispatch-sync [:initialize])     ;; puts a value into application state
  (reagent/render [ui]              ;; mount the application's ui into '<div id="app" />'
                  (js/document.getElementById "app")))
