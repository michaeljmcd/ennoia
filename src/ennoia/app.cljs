(ns ennoia.app
  (:require [reagent.core :as reagent]
            [re-frame.core :as rf]
            [clojure.string :as str]
            [taoensso.timbre :as timbre :refer [log trace debug info with-level]]
            [ennoia.localization :as loc :refer [tr]]
            [ennoia.concept-map :as cm]
            ))

; View Functions
(defn concept-map []
  (cm/layout->ssvg @(rf/subscribe [:current-map]) 300 150 
    :selected-node-id @(rf/subscribe [:selected-node-id])
    :node-creation-callback 
      #(assoc-in % [1 :on-click] 
          (fn [ev] (rf/dispatch [:select-node (get-in % [1 :id])]))
          )))

(def key-bindings 
 {:concept-map-view
  {:insert #(rf/dispatch [:create-node %])
   :f2 #(rf/dispatch [:edit-node-label %])
   :unknown #(do true)
  }})

(defn key-event->symbol [ev]
 (cond
  (= (.-key ev) "Insert") :insert
  (= (.-key ev) "F2") :f2
  :else :unknown
 ))

(defn handle-key-up-events [ev]
 (let [key-symbol (key-event->symbol ev)
       handler (get-in key-bindings [:concept-map-view key-symbol])]
  (if (nil? handler)
   true
   (apply handler [ev])
  ))
)

(defn app-header []
 [:nav
    [:div {:class ["nav-wrapper"]}
        [:a {:href "#" :class "brand-logo"} (tr [:chrome/title])]
        ]]
        )

(defn ui []
 [:div {:on-key-up handle-key-up-events}
  [app-header]
  [concept-map]])

; Event handlers

(rf/reg-event-db 
 :create-concept-map
 (fn [_ _]
  (let [blank (cm/concept-map->layout (cm/create-concept-map))]
      {:maps {(:id blank) blank} 
       :current-map-id (:id blank) 
       :selected-node-id (-> blank :nodes keys first)}
 )))

(defn get-current-map [db]
(let [id (:current-map-id db)]
    (get-in db [:maps id])
    ))

(rf/reg-event-db
 :create-node
 (fn [db _]
  (debug "Original state" db)
  (debug "Original map" (get-current-map db))
  (info "Creating new node in concept map.")

  (let [current-map (get-current-map db)
        new-node (cm/create-node)
        new-edge (cm/create-edge (-> db :selected-node-id) (:id new-node))
        new-map (-> current-map 
                    (cm/add-node new-node) 
                    (cm/add-edge new-edge)
                    cm/concept-map->layout)
        new-state (assoc-in db [:maps (:current-map-id db)] new-map)]
   (debug "New map:" new-map)
   (debug "New state:" new-state)
   new-state)
 ))

(rf/reg-event-db
 :edit-node-label
 (fn [db _]
  db
 ))

(rf/reg-event-db
 :select-node 
 (fn [db [_ id]] (assoc db :selected-node-id id)))

(rf/reg-sub
  :current-map
  (fn [db _]
   (get-current-map db)
   ))

(rf/reg-sub
  :selected-node-id
  (fn [db _]
   (:selected-node-id db)
  ))

(defn ^:export run []
 (with-level :info
  (rf/dispatch-sync [:create-concept-map])     ;; puts a value into application state

  (.addEventListener js/document "keyup" handle-key-up-events false)

  (reagent/render [ui]              ;; mount the application's ui into '<div id="app" />'
                  (js/document.getElementById "app"))))
