(ns ennoia.localization
 (:require [taoensso.tempura :as tempura]))

(def ennoia-dictionary
 { :en { :missing "N/A"
    :chrome { :title "Ennoia" }}
 })

(def tr (partial tempura/tr {:dict ennoia-dictionary} [:en]))

