(ns precos.core
    (:require [reagent.core :as reagent :refer [atom]]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]))

(enable-console-print!)

;; -------------------------
;; Estado
(defonce produtos (atom []))
(defonce visao (atom []))
(defonce cache-produto (atom ""))
(defonce cache-valor (atom ""))
(defonce resultado (atom ""))

;; -------------------------
;; Funcoes
(defn formata [p]
  (str "'" p "'"))

(defn cadastra [] 
  (let [p {:produto @cache-produto :valor @cache-valor}]
    (do
      (swap! produtos conj p)
      (reset! resultado (str (formata (:produto p)) " cadastrado com sucesso.")))))

(defn filtra []
  (do
    (reset! visao (filter #(= @cache-produto (:produto %)) @produtos))
    (reset! resultado (str "Mostrando " (formata @cache-produto) ":"))
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
  [:table  {:border 2}
   [:tr [:td "Produto"] [:td "Preco"]]
   (for [v @visao]
     [:tr [:td (:produto v)] [:td (:valor v)]])])

(defn debug []
  [:div [:label "DEBUG ABAIXO"] 
   [:div [:label (str "produto: " @cache-produto)]] 
   [:div [:label (str "valor: " @cache-valor)]]
   [:div [:label (str "produtos: " @produtos)]]
   [:div [:label (str "visao: " @visao)]]
])

;; -------------------------
;; Views

(defn home-page []
  [:div
   [:div [:label "Produto"](input-element "p" "p" "input" cache-produto) ]
   [:div [:label "Valor"] (input-element "v" "v" "input" cache-valor)]
   [:div
    [:input {:type :button :value "Cadastra" :on-click #(cadastra)}]
    [:input {:type :button :value "Filtra" :on-click #(filtra)}]
    [:label @resultado]
    ]
   [:div [tabela]]
   [:div [debug]]
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
