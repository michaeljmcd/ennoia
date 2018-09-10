(ns ennoia.concept-map
 (:require [ennoia.localization :as l]
           [clojure.math.combinatorics :as combo]
           [taoensso.timbre :as timbre :refer [log debug info with-level]]
  ))

(def default-rectangle-width 50)
(def default-rectangle-height 20)
(def default-shape :rectangle)

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
 {:label (or label (l/tr [:concept-map/blank-concept])) 
  :id (generate-random-uuid)
  :shape default-shape})

(defn create-concept-map []
 (let [concept (create-node)]
   { :nodes {(:id concept) concept} :edges [] :id (generate-random-uuid) }
 ))

; All layout code below assumes the following:
; 1. Standard coordinate system (origin at top left, increasing right and down).
; 2. A bounding rectangle can be drawn around all stencils used.

(defn find-starting-temperature [concept-map]
 1) ; TODO change this

(defn calculate-join-points [bounding-box]
 bounding-box
 )

(defn calculate-bounding-box [x y width height]
 "Takes (x, y) for a point (presumed to be the center of the shape) and calculates a bounding box."
 (->> 
 {:top-left { :x (- x (/ width 2)) :y (- y (/ height 2))}
  :bottom-right { :x (+ x (/ width 2)) :y (+ y (/ height 2))}
  :center-x x
  :center-y y
  :width width
  :height height}
 calculate-join-points))

(defn center-node [node width height]
 (let [center-x (/ width 2)
       center-y (/ height 2)]
   [(:id node) 
    (assoc node :bounding-box 
     (calculate-bounding-box center-x 
                             center-y 
                             default-rectangle-width
                             default-rectangle-height
                             ))
   ]
))

(defn calculate-edges "Calculates edge start-end points. Assumes nodes have already been placed." 
  [nodes edges]
  (->> edges
   (map #(assoc % :start-x 
                  (->> % :from (get nodes) :bounding-box :center-x)
                  :start-y
                  (->> % :from (get nodes) :bounding-box :center-y)
                  :end-x
                  (->> % :to (get nodes) :bounding-box :center-x)
                  :end-y
                  (->> % :to (get nodes) :bounding-box :center-y)
                  ))

  ))

(defn randomly-place-node [node width height]
    (assoc node :bounding-box 
     (calculate-bounding-box (rand-int width)
                             (rand-int height)
                             default-rectangle-width
                             default-rectangle-height
                             )))

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

(defn calculate-node-distance-factor [state]
(let [node-distance-coefficient 3.0
      nodes (-> state :nodes vals)
      node-pairs (combo/cartesian-product nodes nodes)]
    (reduce + 0
     (map #(let [distance (euclidean-distance (-> % first :bounding-box :center-x)
                                              (-> % first :bounding-box :center-y)
                                              (-> % second :bounding-box :center-x)
                                              (-> % second :bounding-box :center-y))]
            (if (= distance 0)
             0
             (* (/ node-distance-coefficient (power distance 2)) distance))
            ) node-pairs))
    ))

(defn overlaps?
 "Determines whether or not two placed nodes overlap."
 [n1 n2]
 ; https://stackoverflow.com/questions/306316/determine-if-two-rectangles-overlap-each-other
 (let [r1 {:x1 (-> n1 :bounding-box :top-left :x) 
           :y1 (-> n1 :bounding-box :top-left :y)
           :x2 (-> n1 :bounding-box :bottom-right :x) 
           :y2 (-> n1 :bounding-box :bottom-right :y)}
        r2 {:x1 (-> n2 :bounding-box :top-left :x) 
            :y1 (-> n2 :bounding-box :top-left :y)
            :x2 (-> n2 :bounding-box :bottom-right :x) 
            :y2 (-> n2 :bounding-box :bottom-right :y)}]
  (debug "Checking for overlap between" n1 n2)
  (debug "Checking bounding boxes" r1 r2)
    (and (< (:x1 r1) (:x2 r2))
         (> (:x2 r1) (:x1 r2))
         (< (:y1 r1) (:y2 r2))
         (> (:y2 r1) (:y1 r2)))
))

(defn calculate-node-overlap-factor [state]
 (let [node-overlap-coefficient 1000
      nodes (-> state :nodes vals)
      node-pairs (combo/cartesian-product nodes nodes)]
    (reduce + 0
     (map #(do 
             (* node-overlap-coefficient 
              (if (overlaps? (first %) (second %)) 1 0)))
      node-pairs)
     )
 ))

(def calculate-state-energy (memoize (fn [state]
    ; TODO: make this more robust
    (+ (calculate-node-distance-factor state)
       (calculate-node-overlap-factor state))
    )))

(def accept-proposed-new-state? (memoize (fn [data new-state]
    (let [original-energy (calculate-state-energy (:current-state data))
          new-energy (calculate-state-energy new-state)]
        (or (< new-energy original-energy)
  ;          (< (rand) (power E (/ (- original-energy new-energy) (:temperature data))))
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
     (timbre/debug "Reached Simulated Annealing termination condition. Returning last calculated state of" data)
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
       max-iterations 30
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

(def effect-definitions 
 '[:defs 
;      [:filter {:id "node-shadow" :x "-100%" :y "-100%" :width "300%" :height "300%"}
;       [:feGaussianBlur {:in "SourceAlpha" :std-deviation "2"}]
;       [:feOffset {:dx "5" :dy "5" :result "offsetblur"}]
;       [:feFlood {:flood-color "gray"}]
;       [:feComposite {:in2 "offsetblur" :operator "in"}]
;       [:feMerge [:feMergeNode] [:feMergeNode {:in "SourceGraphic"}]]
;      ]
        [:linearGradient {:id "Gradient-1" :x1 "20%" :y1 "30%" :x2 "40%" :y2 "80%"}
            [:stop {:offset "0%" :stop-color "#B8D0DE"}]
            [:stop {:offset "100%" :stop-color "#73A2BD"}]
            ]
        [:filter {:id "node-shadow" :xmlns "http://www.w3.org/2000/svg" :height "130%" :width "130%"}
            [:feGaussianBlur {:in "SourceAlpha" :stdDeviation "3"}]
            [:feOffset {:dx "2" :dy "2" :result "offsetblur"}]
            [:feComponentTransfer
                [:feFuncA {:type "linear" :slope "0.2"}]
                ]
            [:feMerge
                [:feMergeNode]
                [:feMergeNode {:in "SourceGraphic"}]
                ]]
 ])

(defn layout->ssvg [layout width height]
 `[:svg {:viewBox ~(str "0 0 " width " " height) 
         :xmlns "http://www.w3.org/2000/svg"}
    ~@(map #(let [x (-> % :bounding-box :top-left :x)
                  y (-> % :bounding-box :top-left :y)
                  width (-> % :bounding-box :width)
                  height (-> % :bounding-box :height)]
            [:g
                [:rect {:x x
                        :y y
                        :width width
                        :height height
;                        :style {:filter "url(#node-shadow)"}
                        :stroke "black" 
                        :fill "white"}]
            [:foreignObject {:width width
                             :height height
                             :x x 
                             :y y}
                             [:div 
                                 {:xmlns "http://www.w3.org/1999/xhtml" 
                                  :style {:color "black" 
                                          :overflow "hidden" 
                                          :width (str width "px") 
                                          :height (str height "px") 
                                          :font-size "8px" 
                                          :padding "5px 5px 5px 5px"}}
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
 (let [validated-width (or width 300)
       validated-height (or height 100)
       layout (simulated-annealing-layout concept-map validated-width validated-height)]
       ; TODO: annotate result so that we have SVG, annotated layout and
       ; algorithm metadata.
    (info "Laying out map" concept-map "and layout" layout)
  (layout->ssvg layout validated-width validated-height)
 )
)
