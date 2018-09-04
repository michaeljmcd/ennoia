(ns ennoia.localization
 (:require [taoensso.tempura :as tempura]))

(def ennoia-dictionary
 { :en { :missing "N/A"
    :chrome { :title "Ennoia" }
    :conceptmap { :blank-concept "Concept 1" }}
 })

(def tr (partial tempura/tr {:dict ennoia-dictionary} [:en]))

