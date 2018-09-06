(ns ennoia.concept-map-test
   (:require [cljs.test :refer-macros [deftest is testing run-tests]]
             [taoensso.timbre :as timbre :refer [log trace info with-level]]
             [ennoia.conceptmap :as cm]))

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
  (is (not (empty? (:nodes m1))))
 ))

(def test-concept-map (let [n1 (cm/create-node :label "n1")
       n2 (cm/create-node :label "n2")
       e1 (cm/create-edge (:id n1) (:id n2))
       cm (-> (cm/create-concept-map)
              (cm/add-node n1)
              (cm/add-node n2)
              (cm/add-edge e1))]
    cm
 ))

(deftest simulated-annealing-tests 
 (with-level :error
     (let [svg-sexp (cm/cm->svg test-concept-map)]
      (trace svg-sexp)
      (is (not (nil? svg-sexp)))
     )

    (let [data {:energy 0 :max-iterations 100 :iteration-number 3}]
     (is (= true (cm/annealing-termination-conditions-met? data))))
))
