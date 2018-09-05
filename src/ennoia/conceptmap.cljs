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

(defn find-starting-temperature [conceptmap]
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

(defn find-starting-state [conceptmap width height]
 ; TODO: we really want this to be account for historical renderings
 (let [nodes (center-nodes (vals (:nodes conceptmap)) width height)
       edges (calculate-edges nodes (:edges conceptmap))]
 { :nodes nodes :edges edges }
))

(defn simulated-annealing-layout-fn [conceptmap temperature currentstate iteration max-iterations width height]
 ;(if (>= iteration max-iterations)
    currentstate
 ;)
)

(defn simulated-annealing-layout [conceptmap width height]
 (let [temperature (find-starting-temperature conceptmap)
       max-iterations 10
       iteration 1
       current-state (find-starting-state conceptmap width height)
  layout (simulated-annealing-layout-fn conceptmap
                                 temperature
                                 max-iterations
                                 iteration
                                 current-state
                                 width
                                 height)]
  layout
 ))

(defn layout->ssvg [layout width height]
; [:svg {:viewBox "0 0 300 100" :xmlns "http://www.w3.org/2000/svg"}
;    [:circle {:cx "50" :cy "50" :r "40" :stroke "red" :fill "grey"}]
;    [:circle {:cx "150" :cy "50" :r "4" :stroke "red" :fill "grey"}]
;  
;    [:svg {:viewBox "0 0 10 10" :x "200" :width "100"}
;      [:circle {:cx "5" :cy "5" :r "4" :stroke "red" :fill "grey"}]
;      ]]
 `[:svg {:viewBox ~(str "0 0 " width " " height) :xmlns "http://www.w3.org/2000/svg"}
    ~@(map #(do [:rect {:x (:x %) :y (:y %) :width (:width %) :height (:width %) :stroke "black" :fill "white"}]) (-> layout :nodes vals))
    ~@(map #(do [:line {:x1 (:start-x %) :y1 (:start-y %) :x2 (:end-x %) :y2 (:end-y %) :stroke "black"}]) (:edges layout))
 ]
)

(defn cm->svg 
 "Generates hiccup-style SVG from the given Concept Map."
 [conceptmap & {:keys [width height]}]
 (let [v-width (or width 300)
       v-height (or height 100)
       layout (simulated-annealing-layout conceptmap v-width v-height)]
       ; TODO: annotate result so that we have SVG, annotated layout and
       ; algorithm metadata.
  (layout->ssvg layout v-width v-height)
 )
)
