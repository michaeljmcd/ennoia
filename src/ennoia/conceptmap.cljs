(ns ennoia.conceptmap
 (:require [ennoia.localization :as l]))

(defn add-node [conceptmap node]
 (assoc-in conceptmap [:nodes (:id node)] node))

(defn add-edge [conceptmap edge]
 (assoc-in conceptmap [:edges] (cons edge (:edges conceptmap))))

(defn create-edge [from-id to-id & {:keys [label]}]
 { :label "" :from from-id :to to-id }
)

(defn create-node [& {:keys [label]}]
 { :label (or label (l/tr [:conceptmap/blank-concept])) :id (random-uuid) }
)

(defn create-conceptmap []
 (let [concept (create-node)]
   { :nodes {(:id concept) concept} :edges [] :id (random-uuid) }
 ))

(defn cm->svg 
 "Generates hiccup-style SVG from the given Concept Map."
 [conceptmap]
[:svg {:viewBox "0 0 300 100" :xmlns "http://www.w3.org/2000/svg"}
  [:circle {:cx "50" :cy "50" :r "40" :stroke "red" :fill "grey"}]
  [:circle {:cx "150" :cy "50" :r "4" :stroke "red" :fill "grey"}]

  [:svg {:viewBox "0 0 10 10" :x "200" :width "100"}
    [:circle {:cx "5" :cy "5" :r "4" :stroke "red" :fill "grey"}]
    ]]
)
