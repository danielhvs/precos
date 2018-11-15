(ns precos-mobile.subs
  (:require [re-frame.core :refer [reg-sub]]))

(reg-sub
  :get-greeting
  (fn [db _]
    (:greeting db)))

;; SUBS
(def subss [:mercado :cache-nome :cache-preco :cache-local :produtos :view-id :nome-consultado :feedback :debug])
(doall (map #(reg-sub % (fn [db _] (% db))) subss))
