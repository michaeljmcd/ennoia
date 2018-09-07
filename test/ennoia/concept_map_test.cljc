(ns ennoia.concept-map-test
   (:require 
    #?(:cljs [cljs.test    :as t :refer-macros [is are deftest testing]]
       :clj [clojure.test :as t :refer [is are deftest testing]])
             [taoensso.timbre :as timbre :refer [log debug trace info with-level]]
             [ennoia.concept-map :as cm]))

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
 (with-level :trace
     (let [svg-sexp (cm/cm->svg test-concept-map)]
      (debug svg-sexp)
      (is (not (nil? svg-sexp)))
     )

    (let [data {:energy 0 :max-iterations 100 :iteration-number 3}]
     (is (true? (cm/annealing-termination-conditions-met? data)))
     (is (true? (cm/annealing-termination-conditions-met? (assoc data :iteration-number 100)))))

    (let [n1 {:x 50 :y 50 :height 20 :width 20}
          n2 {:x 48 :y 50 :height 20 :width 20}]
     (is (true? (cm/overlaps? n1 n2)))) 

    (let [n1 {:x 50 :y 50 :height 20 :width 20}
          n2 {:x 50 :y 69 :height 20 :width 20}]
     (is (true? (cm/overlaps? n1 n2)))) 
))
