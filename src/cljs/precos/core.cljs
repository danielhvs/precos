(ns precos.core
    (:require [reagent.core :as reagent :refer [atom]]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]))
(enable-console-print!)

(println "oi mundo")

;; -------------------------
;; Estado
(defonce moedas (atom {:peso {:nome "PES" :preco-cambio 0.33 :preco-loja 0 :carteira 0 :reais-carteira 0}}))

;; -------------------------
;; Componentes
(defn input-valor []
  [:input {:type "text" 
           :on-change #("oi")}])
 
(defn texto [] 
  [:label "TESTE TEXTO" ])

;; -------------------------
;; Views

(defn home-page []
  [:div
   [:div [:label "Produto"] [input-valor]]
   [:div [:label "Valor"] [input-valor]]
   [:div [:button "Cadastrar"]]
])

(defn about-page []
  [:div [:h2 "About precos"]
   [:div [:a {:href "/"} "go to the home page"]]])

;; -------------------------
;; Routes

(defonce page (atom #'home-page))

(defn current-page []
  [:div [@page]])

(secretary/defroute "/" []
  (reset! page #'home-page))

(secretary/defroute "/about" []
  (reset! page #'about-page))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler
     (fn [path]
       (secretary/dispatch! path))
     :path-exists?
     (fn [path]
       (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (mount-root))
