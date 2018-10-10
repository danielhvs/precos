(ns precos.core
    (:require [reagent.core :as reagent :refer [atom]]
              [secretary.core :as secretary :include-macros true]
              [cljs-time.format :as f]
              [cljs-time.local :as l]
              [accountant.core :as accountant]))

(enable-console-print!)

;; -------------------------
;; Estado
(defonce produtos (atom []))
(defonce cache-produto (atom ""))
(defonce cache-preco (atom ""))
(defonce cache-local (atom ""))
(defonce produtos-mercado 
[
{:produto "queijo" :comprar false}
{:produto "arroz" :comprar true}
{:produto "feijao" :comprar false}
])
(defonce mercado (atom produtos-mercado))

;; -------------------------
;; Funcoes
(defn formata [prefixo p sufixo]
  (str prefixo p sufixo))
(defn formata-aspas [p]
  (formata "'" p "'"))
(defn formata-reais [p]
  (formata "R$ " p ""))
(defn formata-data [p]
  (f/unparse (f/formatter "DD/MM/yyyy hh:mm:ss") p))
(defn ->reais [p]
  (double p))
(defn cadastra [] 
  (let [p {:produto @cache-produto :preco @cache-preco :data (l/local-now) :local @cache-local}]
    (do
      (swap! produtos conj p)
      )))
(defn estilo-botao [p]
  (if (:comprar p) {:background-color "#00FF00"}
    {:background-color "#AA0000"}))
(defn toggle-comprar [p]
  (swap! mercado (fn [a] 
                   (map (fn [i] (if (first (filter #(= (:produto i) (:produto p)) a)) 
                                  (assoc i :comprar (not (:comprar i)))
                                  i)) 
                        a))))

;; -------------------------
;; Componentes
(defn input-element
  "An input element which updates its value on change"
  [id name type value f]
  [:input {:id id
           :name name
           :class "form-control"
           :type type
           :required ""
           :value @value
           :on-change #(reset! value (f (-> % .-target .-value)))}])

(defn colunas-tabela []
  [:tr [:td "Produto"] [:td "Preco"] [:td "Data"] [:td "Local"]])

(defn elemento [v]
  [:tr 
   [:td (formata-aspas (:produto v))] 
   [:td (formata-reais (:preco v))] 
   [:td (formata-data (:data v))] 
   [:td (formata-aspas (:local v))]])

(defn tabela []
  (let [visao (atom []) ]
    (fn []
      (doall
       (reset! visao (filter #(= @cache-produto (:produto %)) @produtos))
       [:div
        (when-let [v (first (sort-by :preco @visao))]
          [:table  {:border 2}
           [:caption "Mais barato"]
           (colunas-tabela)
           (elemento v)])
        (when (not (empty? @visao))
          [:table  {:border 2}
           [:caption "Histórico"]
           (colunas-tabela)
           (for [v @visao]
             (elemento v))])]))))

(defn debug []
  [:div [:label "DEBUG ABAIXO"] 
   [:div [:label (str "produto: " @cache-produto)]] 
   [:div [:label (str "preco: " @cache-preco)]]
   [:div [:label (str "produtos: " @produtos)]]
])

;; -------------------------
;; Views
(defn home-page []
  [:div
   [:div [:label "Produto"] (input-element "p" "p" "input" cache-produto identity) ]
   [:div [:label "Preco"] (input-element "v" "v" "input" cache-preco ->reais)]
   [:div [:label "Local"] (input-element "l" "l" "input" cache-local identity)]
   [:div
    [:input {:type :button :value "Cadastra" :on-click #(cadastra)}]
    (for [p (distinct (map :produto @produtos))]
      [:input {:type :button :value p :on-click #(reset! cache-produto p)}])
    ]
   [:div [tabela]]
   [:div [:a {:href "/lista-compras"} "Lista de compras"]]
   [:div [debug]]
   ])

(defn lista-compras []
  [:div [:h2 "Lista de Compras"]
   (for [p @mercado]
     [:div [:input {:style (estilo-botao p) :type :button :value (:produto p) 
                    :on-click #(toggle-comprar p)} ]])  
   [:div [:a {:href "/"} "Preços dos produtos"]]])

;; -------------------------
;; Routes

(defonce page (atom #'home-page))

(defn current-page []
  [:div [@page]])

(secretary/defroute "/" []
  (reset! page #'home-page))

(secretary/defroute "/lista-compras" []
  (reset! page #'lista-compras))

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
