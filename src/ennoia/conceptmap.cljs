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
   { :nodes {(:id concept) concept} :edges [] }
 ))

