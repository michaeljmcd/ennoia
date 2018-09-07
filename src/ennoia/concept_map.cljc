(ns ennoia.concept-map
 (:require [ennoia.localization :as l]
           [clojure.math.combinatorics :as combo]
           [taoensso.timbre :as timbre :refer [log debug info with-level]]
  ))

(defn add-node [concept-map node]
 (assoc-in concept-map [:nodes (:id node)] node))

(defn add-edge [concept-map edge]
 (assoc-in concept-map [:edges] (cons edge (:edges concept-map))))

(defn create-edge [from-id to-id & {:keys [label]}]
 { :label "" :from from-id :to to-id }
)

(def generate-random-uuid 
#?(:cljs random-uuid :clj java.util.UUID/randomUUID))

(defn create-node [& {:keys [label]}]
 { :label (or label (l/tr [:concept-map/blank-concept])) :id (generate-random-uuid) }
)

(defn create-concept-map []
 (let [concept (create-node)]
   { :nodes {(:id concept) concept} :edges [] :id (generate-random-uuid) }
 ))

(defn find-starting-temperature [concept-map]
 10000) ; TODO change this

(def default-rectangle-width 50)
(def default-rectangle-height 20)
(def default-shape :rectangle)

(defn center-node [node width height]
 (let [center-x (/ width 2)
       center-y (/ height 2)]
   [(:id node) 
         (assoc node :x center-x 
                  :y center-y 
                  :width default-rectangle-width 
                  :height default-rectangle-height 
                  :shape default-shape)]
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

  ))

(defn randomly-place-node [node width height]
 (assoc node :x (rand-int width) 
          :y (rand-int height)
          :shape default-shape 
          :width default-rectangle-width 
          :height default-rectangle-height))

(defn randomly-place-nodes [nodes width height]
 (debug "Randomly placing" nodes)
 (map #(do [(:id %)
        (randomly-place-node % width height)])
  nodes))

(defn find-starting-state [concept-map width height]
 ; TODO: we really want this to account for historical renderings
 (let [original-nodes (vals (:nodes concept-map))
       starting-node (center-node (first original-nodes) width height)
       random-nodes (randomly-place-nodes (rest original-nodes) width height)
       nodes (into {} (cons starting-node random-nodes))
       edges (calculate-edges nodes (:edges concept-map))]
  (debug "Starting state: first node:" starting-node ", random nodes:" random-nodes ",all nodes:" nodes)
 { :nodes nodes :edges edges }
))

(defn annealing-termination-conditions-met? [data]
 (or (<= (:energy data) 0)
     (>= (:iteration-number data) (:max-iterations data))))

(defn calculate-new-state-in-neighborhood [data]
 (let [current-state (:current-state data)
       random-node (rand-nth (-> current-state :nodes vals))
       new-nodes (assoc (:nodes current-state) 
                       (:id random-node) 
                       (randomly-place-node random-node 
                                            (:width data) 
                                            (:height data)))]
 ; Randomly adjusts one randomly selected node.
 ; Restraining the adjustment based on iteration might be a good idea.
 (assoc current-state
        :nodes new-nodes
        :edges (calculate-edges new-nodes (:edges current-state)))
))

(defn square-root [number]
  (#?(:cljs Math/sqrt :clj Math/sqrt)
    number))

(defn power [base exp]
 (#?(:cljs Math/pow :clj Math/pow)
      base exp))

(def E #?(:cljs Math/E :clj Math/E))

(defn euclidean-distance [p1 p2 q1 q2]
 (square-root
     (+ (power (- q1 p1) 2)
        (power (- q2 p2) 2))))

(def calculate-state-energy (memoize (fn [state]
    ; TODO: make this more robust
    (let [node-distance-coefficient 3.0
          nodes (-> state :nodes vals)
          node-pairs (combo/cartesian-product nodes nodes)]
    (reduce + 0
     (map #(let [distance (euclidean-distance (-> % first :x)
                                              (-> % first :y)
                                              (-> % second :x)
                                              (-> % second :y))]
            (if (= distance 0)
             0
             (* (/ node-distance-coefficient (power distance 2)) distance))
            ) node-pairs))
    ))))

(def accept-proposed-new-state? (memoize (fn [data new-state]
    (let [original-energy (calculate-state-energy (:current-state data))
          new-energy (calculate-state-energy new-state)]
        (or (< new-energy original-energy)
            (< (rand) (power E (/ (- original-energy new-energy) (:temperature data))))
        )
    )
)))

(defn decrease-temperature [current-temperature]
 (let [gamma 0.95]
   (* current-temperature gamma)
))

(defn simulated-annealing-layout-fn [data]
 (timbre/debug "Beginning iteration of simulated annealing optimization.")
 (if (annealing-termination-conditions-met? data)
    (do
     (timbre/debug "Reached Simulated Annealing termination condition. Returning last calculated state of" (:current-state data))
     (:current-state data))
    (let [new-state (calculate-new-state-in-neighborhood data)
          data-prime (assoc data :iteration-number (inc (:iteration-number data))
                                 :temperature (decrease-temperature (:temperature data)))]
     (timbre/debug "SA termination condition not reached. Calculated new state of" new-state)
     (timbre/debug "Current state energy is" (calculate-state-energy (:current-state data))
       "Energy of proposed state is" (calculate-state-energy new-state))
     (timbre/debug "Accepting new state?" (accept-proposed-new-state? data new-state))
     (recur (assoc data-prime :current-state 
                              (if (accept-proposed-new-state? data new-state)
                               new-state
                               (:current-state data))))
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
                                 :energy (calculate-state-energy current-state)
                                 :width width
                                 :height height})]
  layout
 ))

(defn layout->ssvg [layout width height]
 `[:svg {:viewBox ~(str "0 0 " width " " height) :xmlns "http://www.w3.org/2000/svg"}
    ~@(map #(let [x (- (:x %) (/ (:width %) 2))
                  y (- (:y %) (/ (:height %) 2))]
            [:g
                [:rect {:x x
                        :y y
                        :width (:width %) 
                        :height (:height %) 
                        :stroke "black" 
                        :fill "white"}]
            [:foreignObject {:width (:width %) 
                             :height (:height %) 
                             :x x
                             :y y}
                             [:div {:xmlns "http://www.w3.org/1999/xhtml" :style {:color "black"}}
                               (:label %)]]
                             ]
            ) 
        (-> layout :nodes vals))
    ~@(map #(do [:line {:x1 (:start-x %) 
                        :y1 (:start-y %) 
                        :x2 (:end-x %) 
                        :y2 (:end-y %) 
                        :stroke "black"}]) 
        (:edges layout))
 ])

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
