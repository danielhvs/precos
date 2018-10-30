(ns precos.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as reagent :refer [atom]]
            [secretary.core :as secretary :include-macros true]
            [cljs-time.format :as f]
            [cljs-time.local :as l]
            [cljs-http.client :as http]
            [cljs.core.async :refer [chan put! <!]]
            [accountant.core :as accountant]))

;; Parse json
(defn json->clj [json] (js->clj (.parse js/JSON json) :keywordize-keys true))
(defn gen-key []
  (gensym "key-"))

;; -------------------------
;; Estado
(defonce produtos (atom []))
(defonce cache-produto (atom ""))
(defonce cache-preco (atom ""))
(defonce cache-local (atom ""))
(defonce a-debug (atom ""))
(defonce mercado (atom #{}))
(defonce resposta (atom ""))
(defonce resposta-cadastro (atom ""))
(defonce resposta-mercado (atom ""))
;(defonce servidor "https://infinite-crag-89428.herokuapp.com/")
(defonce servidor "http://localhost:3000/")


; Feio mas funciona
(def eventos 
  {:toggle-comprar (fn [{:keys [produto comprar estoque]}]
                     (swap! mercado (fn [a] 
                                      (map (fn [i] (if (first (filter #(= (:produto i) produto) a)) 
                                                     (assoc i :comprar (not (:comprar i)))
                                                     i)) 
                                           a))))
   :update-estoque (fn [{:keys [produto comprar estoque]}]
                     (swap! mercado (fn [a] 
                                      (map (fn [i] (if (first (filter #(= (:produto i) produto) a)) 
                                                     (assoc i :estoque estoque)
                                                     i)) 
                                           a))))})

(def canal-eventos (chan))

(go
  (while true 
    (let [[nome-evento dado-evento] (<! canal-eventos)]
      ((nome-evento eventos) dado-evento))))

;; -------------------------
;; Funcoes
(defn formata [prefixo p sufixo]
  (str prefixo p sufixo))
(defn formata-aspas [p]
  (formata "'" p "'"))
(defn formata-reais [p]
  (formata "R$ " p ""))
(defn formata-data [p]
  p)
(defn ->reais [p]
  (double p))
(defn operacao [op] 
  (str servidor op))

(defn salva-mercado []
  (reset! resposta-mercado "Salvando lista de mercado...")
  (go
    (let [response (<! (try (http/post (operacao "salva-mercado") {:json-params @mercado :with-credentials? false})
                            (catch :default e
                              (reset! a-debug e))))]
      (reset! mercado (reverse (sort-by :comprar (json->clj (:body response)))))
      (reset! resposta-mercado ""))))

(defn consulta-mercado []
  (reset! resposta-mercado "Consultando lista de mercado...")
  (go
    (let [response (<! (try (http/get (operacao "consulta-mercado") {:with-credentials? false})
                            (catch :default e
                              (reset! a-debug e))))]
      (reset! mercado (reverse (sort-by :comprar (json->clj (:body response)))))
      (reset! resposta-mercado ""))))

(defn consulta []
  (reset! resposta-cadastro (str "Consultando " @cache-produto "..."))
  (go
    (let [response (<! (try (http/get (operacao (str "consulta/" @cache-produto)) {:with-credentials? false})
                            (catch :default e
                              (reset! a-debug e))))]
      (reset! produtos (json->clj (:body response)))
      (reset! resposta-cadastro ""))))

(defn cadastra [] 
  (reset! resposta-cadastro (str "Cadastrando " @cache-produto "..."))
  (go 
    (let [p {:produto @cache-produto :preco @cache-preco :local @cache-local} 
          response (<! (try (http/post (operacao "cadastra") {:json-params p :with-credentials? false})
                            (catch :default e
                              (reset! a-debug e))))]
      (consulta)
      (consulta-mercado))))

(defn estilo-compra [p]
  (if (:comprar p) 
    {:text-align "center" :background-color "#00FF00" :color "black"}
    {:text-align "center" :background-color "#AA0000" :color "white"}))

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

;; TODO: colunas serem as chaves dos mapas
(defn colunas-tabela []
  [:tr [:td "Produto"] [:td "Preco"] [:td "Data"] [:td "Local"]])

(defn elemento [v] ^{:key (gen-key)}
  [:tr 
   [:td (formata-aspas (:produto v))] 
   [:td (formata-reais (:preco v))] 
   [:td (formata-data (:data v))] 
   [:td (formata-aspas (:local v))]])

(defn tabela []
  (let [visao (atom [])]
    (fn []
      (doall
       (reset! visao @produtos)
       [:div
        #_(when-let [v (first (sort-by :preco @visao))]
          [:table  {:border 2}
           [:caption "Mais barato"]
           [:tbody
            (colunas-tabela)
            (elemento v)]])
        (when (not (empty? @visao))
          [:table  {:border 2}
           [:caption "Histórico"]
           [:tbody
            (colunas-tabela)
            (for [v @visao] ^{:key (gen-key)}
                 (elemento v))]])]))))

(defn debug []
  [:div [:label (str @a-debug)]])

;; -------------------------
;; Views
(defn home-page []
  [:div
   [:div [:a ^{:key (gen-key)} {:href "/lista-compras"} "Lista de compras"]]
   [:div [:h2 "Cadastro"]]
   [:div [:label "Produto"] (input-element "p" "p" "input" cache-produto identity) ]
   [:div [:label "Preco"] (input-element "v" "v" "input" cache-preco ->reais)]
   [:div [:label "Local"] (input-element "l" "l" "input" cache-local identity)]
   [:input {:type :button :value "Cadastra" :on-click #(cadastra)}]
   [:div [:label (str "Produtos " @resposta-mercado)]]
   [:div
    (for [p (distinct (map :produto @mercado))] ^{:key (gen-key)}
         [:input {:type :button :value p :on-click #(do 
                                                      (reset! cache-produto p)
                                                      (consulta))}])]
   [:div [:label "Locais"]]
   [:div
    (for [p (distinct (map :local @produtos))] ^{:key (gen-key)}
         [:input {:type :button :value p :on-click #(reset! cache-local p)}])]
   [:div [:label @resposta-cadastro]]
   [:div [tabela]]
   [:div [debug]]
   ])


(defn estilo-header-tabela []
  {:style {:text-align "center"}})

(defn colunas-tabela-compras []
  [:tr 
   [:td (estilo-header-tabela) "Produto"] 
   [:td (estilo-header-tabela) "Estoque"]])

(defn botao-compra [p]
  [:input {:style (estilo-compra p) 
           :value (:produto p) 
           :on-click #(put! canal-eventos [:toggle-comprar p])}])

(defn entrada-estoque [p]
  [:input {:id "id"
           :name "nome"
           :type "input"
           :value (:estoque p)
           :on-change #(put! canal-eventos [:update-estoque (assoc p :estoque (-> % .-target .-value))])}])

(defn elemento-compras [p] ^{:key (gen-key)}
  [:tr 
   [:td (botao-compra p)] 
   [:td [entrada-estoque p]]])

(defn tabela-compras []
  (fn []
    [:div
     [:table  {:border 0}
      [:caption "Lista de compras"]
      [:tbody
       (colunas-tabela-compras)
       (for [p @mercado] ^{:key (gen-key)}
            (elemento-compras p))]]]))

(defn lista-compras []
  [:div [:a {:href "/"} "Preços dos produtos"]
   [:div 
    [:h2 "Lista de Compras"]
    [:input {:type :button :value "Salva" :on-click #(salva-mercado)}]
    [:div [:label @resposta-mercado]]
    [:label @resposta]
    [tabela-compras]]]) 

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

(enable-console-print!)
(consulta-mercado)
