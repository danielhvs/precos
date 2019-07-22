(ns precos.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as reagent :refer [atom]]
            [secretary.core :as secretary :include-macros true]
            [re-frame.core :as rf]
            [day8.re-frame.http-fx]
            [clojure.string :as str]
            [cljs-time.format :as f]
            [ajax.core :as ajax]
            [cljs-time.local :as l]
            [re-com.buttons :refer [button md-circle-icon-button]]
            [re-com.box :refer [h-box v-box box gap]]
            [re-com.alert :refer [alert-box]]
            [re-com.misc :refer [throbber input-text]]
            [re-com.text :refer [label title]]
            [re-com.tabs :refer [horizontal-tabs]]
            [re-com.core :refer [line]]
            [re-com.modal-panel :refer [modal-panel]]
            [cljs-http.client :as http]
            [goog.string :as gstring]
            [cljs.core.async :refer [<!]]
            [accountant.core :as accountant]))

(declare salva-mercado)
(declare consulta-mercado)
(declare consulta)
(declare cadastra)
(declare header)
(declare view-cadastro)
(declare view-precos)
(declare view-historico)
(def servidor "https://infinite-crag-89428.herokuapp.com")
#_(def servidor "http://localhost:3000")

(defonce ^const TIMEOUT_ESCRITA 20000)
(defonce ^const TIMEOUT_LEITURA 30000)

;; -------------------------
;; Funcoes
(defn operacao [op] 
  (str servidor op))

(defn normaliza [nome]
  "Faz kebab-case e remove 'de'"
  (let [palavras
        (filter #(and (not (empty? %)))
                (map str/lower-case (str/split nome #" ")))]
    (str/replace 
     (reduce #(str %1 "-" %2) palavras)
     #"-de-"
     "-")))

(defn estilo-compra [p]
  (if (:comprar p) 
    {:background-color "coral"}
    {:background-color "skyblue"}))

(defn formata-data [p]
  p)
(defn nao-tem-preco [preco]
  (or (nil? preco)
   (> preco 999998)))
(defn formata-preco [preco]
  (if (nao-tem-preco preco) 
    "-"
    (gstring/format "R$ %.2f" preco)))


;; EVENTS
(defn registra-feedback [db chave valor]
  (assoc db 
    :feedback (assoc (:feedback db) chave valor)))

(rf/reg-event-db
 :falha-http
 (fn [db [_ result]]
   (registra-feedback db :resposta (str "Erro: " (:status-text result)))))

(rf/reg-event-db
 :sucesso-insere-historico
 (fn [db [_ result]]
   (do
     (rf/dispatch [:consulta-historico (:cache-nome db)])
     (registra-feedback db :resposta ""))))

(rf/reg-event-db
 :sucesso-insere-sumario
 (fn [db [_ result]]
   (do
     (rf/dispatch [:consulta-produtos])
     (registra-feedback db :resposta ""))))

(rf/reg-event-db
 :sucesso-produtos
 (fn [db [_ result]]
   (assoc
       (registra-feedback db :resposta "")
     :produtos result)))

(rf/reg-event-db
 :sucesso-consulta-historico
 (fn [db [_ chave result]] 
   (do
     (rf/dispatch [:altera-view view-historico])
     (assoc
         (registra-feedback db :resposta "")
       :historico result
       :cache-nome chave))))

(rf/reg-event-fx 
 :insere-sumario
 (fn [{:keys [db]} _] 
   (let [nome (:cache-nome db)]
     {:db (registra-feedback db :resposta "Insere....")
      :http-xhrio {:method :post
                   :uri (operacao (str "/produtos/" nome "/sumario"))
                   :params {:sumario (:cache-sumario db)} 
                   :format (ajax/json-request-format)
                   :timeout TIMEOUT_ESCRITA
                   :response-format (ajax/text-response-format)
                   :on-success [:sucesso-insere-sumario]
                   :on-failure [:falha-http]}})))

(rf/reg-event-fx 
 :insere-historico
 (fn [{:keys [db]} _] 
   (let [nome (:cache-nome db)]
     {:db (registra-feedback db :resposta "Insere....")
      :http-xhrio {:method :post
                   :uri (operacao (str "/produtos/" nome "/historico"))
                   :params {:preco (:cache-preco db) :local (:cache-local db) :obs (:cache-obs db)} 
                   :format (ajax/json-request-format)
                   :timeout TIMEOUT_ESCRITA
                   :response-format (ajax/text-response-format)
                   :on-success [:sucesso-insere-historico]
                   :on-failure [:falha-http]}})))

(rf/reg-event-fx 
 :consulta-historico
 (fn [{:keys [db]} [_ nome]] 
   {:db (assoc db :resposta (str "Consultando " nome "..."))
    :http-xhrio {:method :get
                 :uri (operacao (str "/produtos/" nome))
                 :timeout TIMEOUT_LEITURA
                 :response-format (ajax/json-response-format {:keywords? true})
                 :on-success [:sucesso-consulta-historico nome]
                 :on-failure [:falha-http]}} ))


(rf/reg-event-fx 
 :consulta-produtos
 (fn [{:keys [db]} _] 
   {:db (registra-feedback db :resposta "Consultando produtos...")
    :http-xhrio {:method :get
                 :uri (operacao "/produtos")
                 :timeout TIMEOUT_LEITURA
                 :response-format (ajax/json-response-format {:keywords? true})
                 :on-success [:sucesso-produtos]
                 :on-failure [:falha-http]}}))

(rf/reg-event-db
 :altera-view 
 (fn [db [_ novo]] 
   (assoc db :view-id novo)))

(rf/reg-event-db
 :initialize
 (fn [_ _]
   {:view-id view-precos :feedback  {}
    :debug {:servidor servidor}})) 

(rf/reg-event-db :cache-nome (fn [db [_ nova-cache]] (assoc db :cache-nome (normaliza nova-cache))))
(rf/reg-event-db :cache-local (fn [db [_ nova-cache]] (assoc db :cache-local nova-cache)))
(rf/reg-event-db :cache-obs (fn [db [_ nova-cache]] (assoc db :cache-obs nova-cache)))
(rf/reg-event-db :cache-sumario (fn [db [_ nova-cache]] (assoc db :cache-sumario nova-cache)))
(rf/reg-event-db :cache-preco (fn [db [_ nova-cache]] (assoc db :cache-preco
                                                             (if (str/includes? nova-cache ".") 
                                                               nova-cache
                                                               (gstring/format "%.2f" (/ (js/parseInt nova-cache) 100))))))
(rf/reg-event-db :produtos (fn [db [_ novo]] (assoc db :produtos novo)))


;; SUBS
(def subss [:cache-nome :cache-preco :cache-local :produtos :view-id :nome-consultado :feedback :debug :historico :cache-obs :cache-sumario])
(doall (map #(rf/reg-sub % (fn [db _] (% db))) subss))

;; VIEW
(defn gen-key []
  (gensym "key-"))

;; -------------------------
;; Componentes
(defn feedback []
  (let [feedback (rf/subscribe [:feedback])]
    (if (not (empty? @feedback))
      [label :label [:div (for [k (keys @feedback)]
                               (k @feedback))]]
      [:div])))

(defn input-element
  [value funcao placeholder f]
  [input-text :width "8em"
   :placeholder placeholder
   :model (str @(rf/subscribe [value]))
   :on-change #(rf/dispatch [funcao (f %)])])

;; -------------------------
;; Views
(defn titulo [t l]
  [title :underline? true :level l :label t])

(defn header []
  (let [view-id (rf/subscribe [:view-id])
        active? #(= @view-id %)
        ->class #(if (= @view-id %) "active" "")]
    [:div
     [:ul {:class "menu"}
      [:li [:button {:on-click #(rf/dispatch [:altera-view view-precos]) :class (->class view-precos)} "Precos"]]
      #_[:li [:button {:on-click #(rf/dispatch [:altera-view view-estoque]) :class (->class view-estoque)} "Estoque" ]]
      #_[:li [:button {:on-click #(rf/dispatch [:altera-view view-cadastro]) :class (->class view-cadastro)} "Cadastro" ]]
      ]])
)

(defn pagina-toda []
  (let [view (rf/subscribe [:view-id])]
    [:div
     [@view]]))

(defn template [botao conteudo]
  [v-box :gap "10px" 
   :children [[gap :size "10px"]
              [h-box
               :children [[gap :size "10px"]
                          [v-box :gap "10px"
                           :children [[h-box :gap "10px" 
                                       :children [botao
                                                  [feedback]
                                                  ]]
                                      [line :size "3px" :color "green"]
                                      conteudo
                                      ]]]]]])

(defn view-historico [] 
  (let [historico (rf/subscribe [:historico])
        nome-atual (rf/subscribe [:cache-nome])]
    (template 
     [button :label "<" :class "btn-primary" :on-click #(rf/dispatch [:altera-view view-precos])]
     [:div
      [titulo @nome-atual :level2]
      [label :label (:sumario @historico)]
      [:table
       [:tbody
        (for [h (:historico @historico)]
          [:tr
           [:td (str (formata-preco (:preco h)))]
           [:td (str (:local h))]
           [:td (str (:obs h))]
           ])]]]
     )
))

(defn view-precos [] 
  (let [produtos (rf/subscribe [:produtos])]
    (template 
     [button :label "Consulta" :class "btn-primary" :on-click #(rf/dispatch [:consulta-produtos])]
     [:table
      [:tbody
       (for [p @produtos]
         [:tr
          [button :label (:nome p) :class "btn-link" :on-click #(rf/dispatch [:consulta-historico (:nome p)])]
          [:td (formata-preco (:melhor-preco p))]
          [:td (:sumario p)]
          ])]])))

;; -------------------------
;; Routes

(defonce page (atom #'pagina-toda))

(defn current-page []
  [:div [@page]])

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
