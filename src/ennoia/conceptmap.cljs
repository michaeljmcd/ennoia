(ns ennoia.conceptmap
 (:require [ennoia.localization :as l]
           [taoensso.timbre :as timbre :refer [log debug info with-level]]
  ))

(defn add-node [concept-map node]
 (assoc-in concept-map [:nodes (:id node)] node))

(defn add-edge [concept-map edge]
 (assoc-in concept-map [:edges] (cons edge (:edges concept-map))))

(defn create-edge [from-id to-id & {:keys [label]}]
 { :label "" :from from-id :to to-id }
)

(defn create-node [& {:keys [label]}]
 { :label (or label (l/tr [:concept-map/blank-concept])) :id (random-uuid) }
)

(defn create-concept-map []
 (let [concept (create-node)]
   { :nodes {(:id concept) concept} :edges [] :id (random-uuid) }
 ))

(defn find-starting-temperature [concept-map]
 10000)

(defn center-nodes [nodes width height]
 (let [center-x (/ width 2)
       center-y (/ height 2)
       width 10 ; TODO
       height 10
       shape :rectangle]
 (->> nodes
      (map #(do [(:id %) (assoc % :x center-x :y center-y :width width :height height :shape shape)]))
      (into {}))
))

(defn calculate-edges "Calculates edge start-end points. Assumes nodes have already been placed." 
  [nodes edges]
  (->> edges
   (map #(assoc % :start-x 
                  (->> % :from (get nodes) :x)
                  :start-y
                  (->> % :from (get nodes) :y)
                  :end-x
                  (->> % :to (get nodes) :x)
                  :end-y
                  (->> % :to (get nodes) :y)
                  ))

  )
  )

(defn find-starting-state [concept-map width height]
 ; TODO: we really want this to account for historical renderings
 (let [nodes (center-nodes (vals (:nodes concept-map)) width height)
       edges (calculate-edges nodes (:edges concept-map))]
 { :nodes nodes :edges edges }
))

(defn annealing-termination-conditions-met? [data]
 (or (>= (:iteration-number data) (:max-iterations data)))
)

(defn calculate-new-state-in-neighborhood [data]
 (:current-state data)
)

(defn simulated-annealing-layout-fn [data]
 (timbre/debug "Beginning iteration of simulated annealing optimization.")
 (if (annealing-termination-conditions-met? data)
    (do
     (timbre/debug "Reached Simulated Annealing termination condition. Returning last calculated state of" (:current-state data))
     (:current-state data))
    (let [new-state (calculate-new-state-in-neighborhood data)
          data-prime (assoc data :iteration-number (inc (:iteration-number data)))]
     (timbre/debug "SA termination condition not reached. Calculated new state of" new-state)
     (recur data-prime)
    ))
)

(defn simulated-annealing-layout [concept-map width height]
 (let [temperature (find-starting-temperature concept-map)
       max-iterations 10
       iteration 1
       current-state (find-starting-state concept-map width height)
  layout (simulated-annealing-layout-fn {:concept-map concept-map
                                 :temperature temperature
                                 :current-state current-state
                                 :iteration-number iteration
                                 :max-iterations max-iterations
                                 :width width
                                 :height height})]
  layout
 ))

(defn layout->ssvg [layout width height]
 `[:svg {:viewBox ~(str "0 0 " width " " height) :xmlns "http://www.w3.org/2000/svg"}
    ~@(map #(do [:rect {:x (:x %) :y (:y %) :width (:width %) :height (:width %) :stroke "black" :fill "white"}]) (-> layout :nodes vals))
    ~@(map #(do [:line {:x1 (:start-x %) :y1 (:start-y %) :x2 (:end-x %) :y2 (:end-y %) :stroke "black"}]) (:edges layout))
 ]
)

(defn cm->svg 
 "Generates hiccup-style SVG from the given Concept Map."
 [concept-map & {:keys [width height]}]
 (let [v-width (or width 300)
       v-height (or height 100)
       layout (simulated-annealing-layout concept-map v-width v-height)]
       ; TODO: annotate result so that we have SVG, annotated layout and
       ; algorithm metadata.
    (info "Laying out map" concept-map "and layout" layout)
  (layout->ssvg layout v-width v-height)
 )
)
