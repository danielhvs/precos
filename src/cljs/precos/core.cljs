(ns precos.core
    (:require [reagent.core :as reagent :refer [atom]]
              [secretary.core :as secretary :include-macros true]
              [cljs-time.core :as t]
              [cljs-time.format :as f]
              [accountant.core :as accountant]))

(enable-console-print!)

;; -------------------------
;; Estado
(defonce produtos (atom []))
(defonce cache-produto (atom ""))
(defonce cache-valor (atom ""))
(defonce cache-local (atom ""))

;; -------------------------
;; Funcoes
(defn formata [prefixo p sufixo]
  (str prefixo p sufixo))

(defn aspas [p]
  (formata "'" p "'"))
(defn reais [p]
  (formata "R$" p ""))

(defn cadastra [] 
  (let [p {:produto @cache-produto :valor @cache-valor :data (t/now) :local @cache-local}]
    (do
      (swap! produtos conj p)
      )))

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

(defn elemento [v]
  [:tr 
   [:td (aspas (:produto v))] 
   [:td (reais (:valor v))] 
   [:td (f/unparse (f/formatter "DD/MM/yyyy hh:mm:ss") (:data v))] 
   [:td (:local v)]])

(defn tabela []
  (let [visao (atom []) ]
    (fn []
      (doall
        (reset! visao (filter #(= @cache-produto (:produto %)) @produtos))
        [:div 
         (when-let [v (first (sort-by :preco @visao))]
           [:table  {:border 2}
            [:caption "Mais barato"]
            [:tr [:td "Produto"] [:td "Preco"] [:td "Data"] [:td "Local"]]
            (elemento v)])
         (when (not (empty? @visao))
           [:table  {:border 2}
            [:caption "Histórico"]
            [:tr [:td "Produto"] [:td "Preco"] [:td "Data"] [:td "Local"]]
            (for [v @visao]
              (elemento v))])]))))

(defn debug []
  [:div [:label "DEBUG ABAIXO"] 
   [:div [:label (str "produto: " @cache-produto)]] 
   [:div [:label (str "valor: " @cache-valor)]]
   [:div [:label (str "produtos: " @produtos)]]
])

;; -------------------------
;; Views
(defn home-page []
  [:div
   [:div [:label "Produto"] (input-element "p" "p" "input" cache-produto) ]
   [:div [:label "Valor"] (input-element "v" "v" "input" cache-valor)]
   [:div [:label "Local"] (input-element "l" "l" "input" cache-local)]
   [:div
    [:input {:type :button :value "Cadastra" :on-click #(cadastra)}]
    (for [p (distinct (map :produto @produtos))]
      [:input {:type :button :value p :on-click #(reset! cache-produto p)}])
    ]
   [:div [tabela]]
   #_[:div [debug]]
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
