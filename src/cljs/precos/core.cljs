(ns precos.core
    (:require [reagent.core :as reagent :refer [atom]]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]))

(enable-console-print!)

;; -------------------------
;; Estado
(defonce produtos (atom []))
(defonce cache-produto (atom ""))
(defonce cache-valor (atom ""))

;; -------------------------
;; Funcoes
(defn cadastra [] 
  (do
    (swap! produtos conj {:produto @cache-produto :valor @cache-valor})
))

;; -------------------------
;; Componentes
(defn input-element
  "An input element which updates its value on change"
  [id name type value]
  [:input {:id id
           :name name
           :class "form-control"
           :type type
           :required ""
           :value @value
           :on-change #(reset! value (-> % .-target .-value))}])

(defn tabela []
  [:table  
   [:tr [:td "Produto"] [:td "Preco"]]
   (for [p @produtos]
     [:tr [:td (:produto p)] [:td (:valor p)]])])

;; -------------------------
;; Views

(defn home-page []
  [:div
   [:div [:label "Produto"](input-element "p" "p" "input" cache-produto) ]
   [:div [:label "Valor"] (input-element "v" "v" "input" cache-valor)]
   [:div [:input {:type :button :value "Cadastra" :on-click #(cadastra)}]]
   [:div [tabela]]
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
