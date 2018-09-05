(ns ennoia.conceptmap-test
   (:require [cljs.test :refer-macros [deftest is testing run-tests]]
             [ennoia.conceptmap :as cm]))

(deftest building-a-conceptmap
 (let [n1 (cm/create-node :label "n1")
       n2 (cm/create-node :label "n2")
       e1 (cm/create-edge (:id n1) (:id n2))
       cm (-> (cm/create-conceptmap)
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

(deftest conceptmap-operations
 (let [m1 (cm/create-conceptmap)]
  (is (not (nil? m1)))
  (is (not (empty? (:nodes m1))))
 ))

(def test-conceptmap (let [n1 (cm/create-node :label "n1")
       n2 (cm/create-node :label "n2")
       e1 (cm/create-edge (:id n1) (:id n2))
       cm (-> (cm/create-conceptmap)
              (cm/add-node n1)
              (cm/add-node n2)
              (cm/add-edge e1))]
    cm
 ))

(deftest simulated-annealing-tests 
 (let [svg-sexp (cm/cm->svg test-conceptmap)]
  (println svg-sexp)
  (is (not (nil? svg-sexp)))
 ))
