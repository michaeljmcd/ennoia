(ns ennoia.concept-map-test
   (:require 
    #?(:cljs [cljs.test    :as t :refer-macros [is are deftest testing]]
       :clj [clojure.test :as t :refer [is are deftest testing]])
             [taoensso.timbre :as timbre :refer [log debug trace info with-level]]
             [ennoia.concept-map :as cm]))

(def test-concept-map (let [n1 (cm/create-node :label "n1")
       n2 (cm/create-node :label "n2")
       e1 (cm/create-edge (:id n1) (:id n2))
       cm (-> (cm/create-concept-map)
              (cm/add-node n1)
              (cm/add-node n2)
              (cm/add-edge e1))]
    cm
 ))

(deftest building-a-concept-map
 (let [n1 (cm/create-node :label "n1")
       n2 (cm/create-node :label "n2")
       e1 (cm/create-edge (:id n1) (:id n2))
       cm (-> (cm/create-concept-map)
              (cm/add-node n1)
              (cm/add-node n2)
              (cm/add-edge e1))]
    (is (not (nil? cm)))
 ))

(deftest node-operations
 (let [n1 (cm/create-node)
       n2 (cm/create-node :label "asdf")]
    (is (= "Concept 1" (:label n1)))
    (is (= "asdf" (:label n2)))
 ))

(deftest concept-map-operations
 (let [m1 (cm/create-concept-map)]
  (is (not (nil? m1)))
  (is (not (empty? (:nodes m1)))))

 (let [m1 (cm/create-concept-map)
       n1 (-> m1 :nodes vals first (assoc :label "asdf"))
       m2 (cm/replace-nodes m1 [n1])]
  (is (not (= m1 m2)))
  (is (not (= "asdf" (-> m1 :nodes vals first :label))))
  (is (= "asdf" (-> m2 :nodes vals first :label)))))

(deftest general-layout-tests
    (let [n1 {:bounding-box {:top-left {:x 50 :y 50} 
                             :bottom-right {:x 70 :y 70}}}
          n2 {:bounding-box {:top-left {:x 48 :y 50} :bottom-right {:x 68 :y 70}}}]
     (is (true? (cm/overlaps? n1 n2)))
     ) 

    (let [n1 {:bounding-box (cm/calculate-bounding-box 60 60 20 20)}
          n2 {:bounding-box (cm/calculate-bounding-box 60 79 20 20)}]
    (is (true? (cm/overlaps? n1 n2)))) 
    )

(deftest layout-area-tests
)

(deftest simulated-annealing-tests 
 (with-level :info
     (let [svg-sexp (cm/cm->svg test-concept-map)]
      (debug svg-sexp)
      (is (not (nil? svg-sexp))))

      (let [m (cm/create-concept-map)
            m2 (cm/simulated-annealing-layout m 300 300)]
          (info "Testing" m " and " m2)
       (is (true? (cm/blank-rendering-slate? m)))
       (is (false? (cm/blank-rendering-slate? m2))))

     (let [m (cm/create-concept-map)
           m2 (cm/place-unplaced-nodes m 300 300)
           c1 (cm/find-unplaced-nodes m)
           c2 (cm/find-unplaced-nodes m2)]
      (is (= 1 (count c1)))
      (is (= 0 (count c2))))

     (let [m1 (cm/place-unplaced-nodes (cm/create-concept-map) 300 300)
           n1 (cm/create-node)
           e1 (cm/create-edge (-> m1 :nodes vals first :id) (:id n1))
           m2 (-> m1 (cm/add-node n1) (cm/add-edge e1))
           result (cm/find-unplaced-nodes m2)]
         (trace "Checking::" m2 "res:" result "original:" m1)
      (is (= 1 (count result))))

    (let [data {:energy 0 :max-iterations 100 :iteration-number 3}]
     (is (true? (cm/annealing-termination-conditions-met? data)))
     (is (true? (cm/annealing-termination-conditions-met? (assoc data :iteration-number 100)))))
))
