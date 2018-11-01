(ns precos.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as reagent :refer [atom]]
            [secretary.core :as secretary :include-macros true]
            [re-frame.core :as rf]
            [clojure.string :as str]
            [cljs-time.format :as f]
            [cljs-time.local :as l]
            [re-com.buttons :refer [button md-circle-icon-button]]
            [re-com.box :refer [h-box v-box box]]
            [re-com.misc :refer [input-text]]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [accountant.core :as accountant]))

(declare salva-mercado)
(declare consulta-mercado)
(declare consulta)
(declare cadastra)
(declare header)

(defn estilo-compra [p]
  (if (:comprar p) 
    {:background-color "coral"}
    {:background-color "skyblue"}))

(rf/reg-event-db              ;; sets up initial application state
  :initialize                 ;; usage:  (dispatch [:initialize])
  (fn [_ _]                   ;; the two parameters are not important here, so use _
    {})) 

(rf/reg-event-db                
  :toggle-comprar
  (fn [db [_ {:keys [nome comprar estoque]}]] 
    (let [mercado (:mercado db)
          item (first (filter #(= nome (:nome %)) mercado))]
      (assoc db :mercado 
             (map (fn [i] 
                    (if (= item i) 
                      (assoc item :comprar (not (:comprar item))) 
                      i)) 
                  mercado)))))

(rf/reg-event-db                
  :update-estoque
  (fn [db [_ {:keys [nome comprar estoque]}]] 
    (let [mercado (:mercado db)
          item (first (filter #(= nome (:nome %)) mercado))]
      (assoc db :mercado 
             (map (fn [i] 
                    (if (= item i) 
                      (assoc item :estoque estoque) 
                      i)) 
                  mercado)))))

(rf/reg-event-db :update-mercado (fn [db [_ novo-mercado]] (assoc db :mercado (filter #(not (nil? (:nome %))) novo-mercado))))
(rf/reg-event-db :resposta-mercado (fn [db [_ nova-resposta]] (assoc db :resposta-mercado nova-resposta)))
(rf/reg-event-db :resposta-cadastro (fn [db [_ nova-resposta]] (assoc db :resposta-cadastro nova-resposta)))
(rf/reg-event-db :cache-produto (fn [db [_ nova-cache]] (assoc db :cache-produto nova-cache)))
(rf/reg-event-db :cache-local (fn [db [_ nova-cache]] (assoc db :cache-local nova-cache)))
(rf/reg-event-db :cache-preco (fn [db [_ nova-cache]] (assoc db :cache-preco nova-cache)))
(rf/reg-event-db :produtos (fn [db [_ novo]] (assoc db :produtos novo)))

(rf/reg-event-db :salva-mercado (fn [db [_ m]] (salva-mercado m) db))
(rf/reg-event-db :consulta-mercado (fn [db [_ _]] (consulta-mercado) db))
(rf/reg-event-db :consulta (fn [db [_ p]] (consulta p) db))
(rf/reg-event-db :cadastra (fn [db [_ p]] (cadastra p) db))

;; -- Domino 4 - Query  -------------------------------------------------------

(rf/reg-sub
  :time
  (fn [db _]     ;; db is current app state. 2nd unused param is query vector
    (:time db))) ;; return a query computation over the application state

(rf/reg-sub :mercado (fn [db _] (:mercado db)))
(rf/reg-sub :resposta-mercado (fn [db _] (:resposta-mercado db)))
(rf/reg-sub :resposta-cadastro (fn [db _] (:resposta-cadastro db)))
(rf/reg-sub :cache-produto (fn [db _] (:cache-produto db)))
(rf/reg-sub :cache-preco (fn [db _] (:cache-preco db)))
(rf/reg-sub :cache-local (fn [db _] (:cache-local db)))
(rf/reg-sub :produtos (fn [db _] (:produtos db)))

;; -- Domino 5 - View Functions ----------------------------------------------

;; Parse json
(defn json->clj [json] (js->clj (.parse js/JSON json) :keywordize-keys true))
(defn gen-key []
  (gensym "key-"))

;; -------------------------
;; Estado
(defonce a-debug (atom ""))
(defonce servidor "https://infinite-crag-89428.herokuapp.com/")
#_(defonce servidor "http://localhost:3000/")

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

(defn salva-mercado [mercado]
  (rf/dispatch [:resposta-mercado "Salvando lista de mercado..."])
  (go
    (let [response (<! (try (http/post (operacao "salva-mercado") {:json-params mercado :with-credentials? false})
                            (catch :default e
                              (reset! a-debug e))))]
      (rf/dispatch [:update-mercado (reverse (sort-by :comprar (json->clj (:body response))))])
      (rf/dispatch [:resposta-mercado ""]))))

(defn consulta-mercado []
  (rf/dispatch [:resposta-mercado "Consultando lista de mercado..."])
  (go
    (let [response (<! (try (http/get (operacao "consulta-mercado") {:with-credentials? false})
                            (catch :default e
                              (reset! a-debug e))))]
      (rf/dispatch [:update-mercado (reverse (sort-by :comprar (json->clj (:body response))))])
      (rf/dispatch [:resposta-mercado ""]))))

(defn consulta [nome]
  (rf/dispatch [:resposta-cadastro (str "Consultando " nome "...")])
  (go
    (let [response (<! (try (http/get (operacao (str "consulta/" nome)) {:with-credentials? false})
                            (catch :default e
                              (reset! a-debug e))))]
      (rf/dispatch [:produtos (json->clj (:body response))])
      (rf/dispatch [:resposta-cadastro ""]))))

(defn cadastra [{:keys [nome preco local]}] 
  (rf/dispatch [:resposta-cadastro (str "Cadastrando " nome "...")])
  (go 
    (let [p {:nome nome :preco preco :local local} 
          response (<! (try (http/post (operacao "cadastra") {:json-params p :with-credentials? false})
                            (catch :default e
                              (reset! a-debug e))))]
      (rf/dispatch [:resposta-cadastro (str "Cadastrado " nome " com sucesso")])
      (rf/dispatch [:consulta nome])
      (rf/dispatch [:consulta-mercado]))))

;; -------------------------
;; Componentes
(defn input-element
  [value funcao f]
  [input-text 
   :model (str @(rf/subscribe [value]))
   :on-change #(rf/dispatch [funcao (f %)])])

(defn colunas-tabela []
  [:tr [:td "Produto"] [:td "Preco"] [:td "Data"] [:td "Local"]])

(defn elemento [v] ^{:key (gen-key)}
  [:tr 
   [:td (formata-aspas (:nome v))] 
   [:td (formata-reais (:preco v))] 
   [:td (formata-data (:data v))] 
   [:td (formata-aspas (:local v))]])

(defn tabela []
  (let [visao (atom [])]
    (fn []
      (doall
       (reset! visao @(rf/subscribe [:produtos]))
       [:div
        #_(when-let [v (first (sort-by :preco @visao))]
          [:table
           [:caption "Mais barato"]
           [:tbody
            (colunas-tabela)
            (elemento v)]])
        (when (not (empty? @visao))
          [:table.resultado
           [:caption "Histórico"]
           [:tbody
            (colunas-tabela)
            (for [v @visao] ^{:key (gen-key)}
                 (elemento v))]])]))))

(defn debug []
  [:div [:label (str @a-debug)]])

(defn menu-compras []
  [:a ^{:key (gen-key)} {:href "/lista-compras"} "Lista de compras"])
(defn menu-cadastro []
  [:a ^{:key (gen-key)} {:href "/"} "Cadastro"])

;; -------------------------
;; Views
(defn view-cadastro []
  [:div
   [:div [:h2 "Cadastro"]]
   [:div [:label "Produto"] (input-element :cache-produto :cache-produto identity) ]
   [:div [:label "Preco"] (input-element :cache-preco :cache-preco ->reais)]
   [:div [:label "Local"] (input-element :cache-local :cache-local identity)]
   (let [p {
            :nome @(rf/subscribe [:cache-produto])
            :local @(rf/subscribe [:cache-local])
            :preco @(rf/subscribe [:cache-preco])
            }]
     [button :class "btn-primary"
      :label "Cadastra" 
      :on-click #(rf/dispatch [:cadastra p])])
   [:div [:label (str "Produtos " @(rf/subscribe [:resposta-mercado]))]]
   [:div
    (for [item @(rf/subscribe [:mercado])] ^{:key (gen-key)}
         [button :style (estilo-compra item) :label (:nome item) :on-click #(do 
                                                                              (rf/dispatch [:cache-produto (:nome item)])
                                                                              (rf/dispatch [:consulta (:nome item)]))])]
   [:div [:label "Locais"]]
   [:div
    (for [p (distinct (map :local @(rf/subscribe [:produtos])))] ^{:key (gen-key)}
         [button :class "btn-secondary" :label p :on-click #(rf/dispatch [:cache-local p])])]
   [:div [:label @(rf/subscribe [:resposta-cadastro])]]
   [:div [tabela]]
   [:div [debug]]
   ]
)

(defn header []
  [h-box :children [[box :child (menu-compras)] 
                    [box :child (menu-cadastro)]]])

(defn home-page []
  [v-box
   :children [(header)
              [box :child (view-cadastro)]]])

(defn estilo-header-tabela []
  {:style {:text-align "center"}})

(defn colunas-tabela-compras []
  [:tr 
   [:td (estilo-header-tabela) "Produto"] 
   [:td (estilo-header-tabela) "Estoque"]])

(defn entrada-estoque [p]
  [:div
   [md-circle-icon-button :size :smaller
    :md-icon-name "zmdi-minus"
    :on-click #(rf/dispatch [:update-estoque (assoc p :estoque (dec (js/parseInt (:estoque p))))])]
   [:label {:style {:padding "12px"}} (:estoque p)]
   [md-circle-icon-button :size :smaller
    :md-icon-name "zmdi-plus"
    :on-click #(rf/dispatch [:update-estoque (assoc p :estoque (inc (js/parseInt (:estoque p))))]) ]])

(defn elemento-compras [p] ^{:key (gen-key)}
  [:tr 
   [:td {:style (estilo-compra p) 
         :on-click #(rf/dispatch [:toggle-comprar p])}
    (:nome p)] 
   [:td [entrada-estoque p]]])

(defn tabela-compras []
  (fn []
    [:div
     [:table.resultado
      [:caption "Lista de compras"]
      [:tbody
       (colunas-tabela-compras)
       (for [p @(rf/subscribe [:mercado])] ^{:key (gen-key)}
            (elemento-compras p))]]]))

(defn lista-compras []
  [v-box :children [(header) 
                    [:div 
                     [:h2 "Lista de Compras"]
                     [:input {:type :button :value "Salva" :on-click #(rf/dispatch [:salva-mercado @(rf/subscribe [:mercado])])}]
                     [:div [:label @(rf/subscribe [:resposta-mercado])]]
                     [tabela-compras]]]]) 

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
  (rf/dispatch-sync [:initialize])     ;; puts a value into application state
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
(rf/dispatch [:consulta-mercado])
