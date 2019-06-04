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
            [re-com.misc :refer [throbber input-text]]
            [re-com.text :refer [label title]]
            [re-com.tabs :refer [horizontal-tabs]]
            [re-com.modal-panel :refer [modal-panel]
             ]
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
(declare view-estoque)
#_(def servidor "https://infinite-crag-89428.herokuapp.com/")
(def servidor "http://localhost:3000/")

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
  (> preco 999998))
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
   (registra-feedback db :resposta-mercado (str "Erro: " (:status-text result)))))

(rf/reg-event-db
 :sucesso-produtos
 (fn [db [_ result]]
   (assoc
       (registra-feedback db :resposta-produtos "Sucesso")
     :produtos result)))

(rf/reg-event-fx 
 :consulta-produtos
 (fn [{:keys [db]} _] 
   {:db (registra-feedback db :resposta-mercado "Consultando produtos...")
    :http-xhrio {:method :get
                 :uri (operacao "produtos")
                 :timeout TIMEOUT_LEITURA
                 :response-format (ajax/json-response-format {:keywords? true})
                 :on-success [:sucesso-produtos]
                 :on-failure [:falha-http]}} ))

(rf/reg-event-db
 :altera-view 
 (fn [db [_ novo]] 
   (assoc db :view-id novo)))

(rf/reg-event-db
 :initialize
 (fn [_ _]
   {:view-id view-precos :feedback  {}
    :debug {:servidor servidor}})) 

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
 (fn [db [_ {:keys [nome comprar estoque i]} x]] 
   (let [mercado (vec (sort-by :nome (:mercado db)))
         item (nth mercado i)]
     (assoc db :mercado (assoc mercado i (assoc item :estoque (js/parseInt x)))))))

(rf/reg-event-db :cache-nome (fn [db [_ nova-cache]] (assoc db :cache-nome (normaliza nova-cache))))
(rf/reg-event-db :cache-local (fn [db [_ nova-cache]] (assoc db :cache-local nova-cache)))
(rf/reg-event-db :cache-info (fn [db [_ nova-cache]] (assoc db :cache-info nova-cache)))
(rf/reg-event-db :cache-preco (fn [db [_ nova-cache]] (assoc db :cache-preco
                                                             (if (str/includes? nova-cache ".") 
                                                               nova-cache
                                                               (gstring/format "%.2f" (/ (js/parseInt nova-cache) 100))))))
(rf/reg-event-db :produtos (fn [db [_ novo]] (assoc db :produtos novo)))


;; SUBS
(def subss [:mercado :cache-nome :cache-preco :cache-local :produtos :view-id :nome-consultado :feedback :debug :historico :resposta-historico :cache-info])
(doall (map #(rf/reg-sub % (fn [db _] (% db))) subss))

;; VIEW
(defn gen-key []
  (gensym "key-"))

;; -------------------------
;; Componentes
(defn box-centro [componente]
  [box :align :center :justify :around :child componente])

(defn feedback []
  (let [feedback (rf/subscribe [:feedback])]
    (when (not (empty? @feedback))
      [:div
       (for [f (keys @feedback)]
         [:div (f @feedback)])])))

(defn input-com-regex [entrada regex]
  (conj entrada :validation-regex regex))

(defn input-element
  [value funcao placeholder f]
  [input-text
   :placeholder placeholder
   :model (str @(rf/subscribe [value]))
   :on-change #(rf/dispatch [funcao (f %)])])

#_(defn input-element
  "An input element which updates its value on change"
  [value funcao placeholder f]
  [:input {:placeholder placeholder
           :class "entrada"
           :value (str @(rf/subscribe [value]))
           :on-change #(rf/dispatch [funcao (f (-> % .-target .-value))])}])

(defn colunas-tabela []
  [:tr 
   [:td "Produto"] 
   [:td "Preco"] 
   [:td "Data"] 
   [:td "Local"] 
   [:td "Observacao"]])

(defn elemento [v] ^{:key (gen-key)}
  [:tr 
   [:td (:nome v)] 
   [:td (formata-preco (:preco v))] 
   [:td (formata-data (:data v))] 
   [:td (:local v)] 
   [:td (:info v)]])

(defn tabela [visao]
  [:div
   (when (not (empty? visao))
     [:table {:class "table"}
      [:tbody
       (colunas-tabela)
       (for [v visao] ^{:key (gen-key)}
         (elemento v))]])])

;; -------------------------
;; Views
(defn botao-consulta-mercado [texto]
  [button :label texto :class "btn-primary" :on-click #(rf/dispatch [:consulta-produtos])])

(defn titulo [t l]
  [title :underline? true :level l :label t])

(defn form-cadastro []
  [:div.espacados-vertical
   [titulo "Cadastro" :level1]
   [feedback]
   [input-element :cache-nome :cache-nome "Produto" identity] 
   [input-com-regex (input-element :cache-preco :cache-preco "Preco" identity) #"^[0-9]*(\.[0-9]{0,2})?$"]
   [input-element :cache-local :cache-local "Local" identity]
   [input-element :cache-info :cache-info "Observacao" identity]
   [gap :size "2em"]
   [:div.espacados-horizontal
    [button :class "btn-primary"
     :label "Cadastra" 
     :on-click #(rf/dispatch [:cadastra {:nome @(rf/subscribe [:cache-nome])
                                         :local @(rf/subscribe [:cache-local])
                                         :info @(rf/subscribe [:cache-info])
                                         :preco @(rf/subscribe [:cache-preco])}])]

    [button :label "Consulta Produto" :class "btn-secondary" :on-click #(rf/dispatch [:consulta @(rf/subscribe [:cache-nome])])]
    
    [botao-consulta-mercado "Atualiza Mercado"]]
   (let [nome-consultado (rf/subscribe [:nome-consultado])]
     (when (seq @nome-consultado)
       (let [produtos (rf/subscribe [:produtos])]
         [:div.espacados-vertical
          [titulo (str "Historico " @(rf/subscribe [:nome-consultado])) :level2]
          [tabela @produtos]
          [titulo (str "Locais " @(rf/subscribe [:nome-consultado])) :level2]       
          (for [p (distinct (map :local @produtos))] ^{:key (gen-key)}
            [button :class "btn-secondary" :label (if (empty? p) "(vazio)!?" p) :on-click #(rf/dispatch [:cache-local p])])

          ])))
   (let [mercado (rf/subscribe [:mercado])]
     (when (seq @mercado)
       [:div
        [titulo "Produtos" :level2]
        [:div.espacados-horizontal
         (for [item @(rf/subscribe [:mercado])] ^{:key (gen-key)}
           [button :style (estilo-compra item) :label (:nome item) :on-click #(rf/dispatch [:cache-nome (:nome item)])])]]))
   ])

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

(defn footer []
  [:div]
  #_[:div.debug (str "DEBUG: " @(rf/subscribe [:debug]))])

(defn pagina-toda []
  (let [view (rf/subscribe [:view-id])]
    [:div.view
     [@view]]))

(defn view-cadastro []
  [:div.espacados-vertical  
   [header]
   [form-cadastro]
   [footer]])

(defn estilo-centro []
  {:text-align "center" :vertical-align "middle"})

(defn entrada-estoque [p]
  [box-centro
   [:div
    [:select {:key "estoque" :on-change #(rf/dispatch [:update-estoque p (.. % -target -value)])}
     (for [x (range 100)]
       (if (= x (:estoque p))
         [:option {:key x :selected "true"} x]
         [:option {:key x} x]))]]])

(defn label-mercado [texto]
  [:td {:style (conj (estilo-centro))}
   [:font {:size 2}] texto])


(defn elemento-estoque [p] ^{:key (gen-key)}
  [:tr 
   [:td [entrada-estoque p]]
   [:td {:style (estilo-centro)}
    [:font {:size 2}] (:nome p)]])

(defn tabela-estoque [mercado]
  [:div
   [:table.table
    [:tbody
     [:tr 
      [:td {:style (estilo-centro)} "Estoque"]
      [:td {:style (estilo-centro)} "Produto"]]
     (for [p (map-indexed (fn [i item] (assoc item :i i)) (sort-by :nome mercado))] ^{:key (gen-key)}
          [elemento-estoque p])]]])

(defn view-estoque []
  (let [mercado (rf/subscribe [:mercado])]
    [:div.espacados-vertical
     [header] 
     [titulo "Estoque" :level1]
     [feedback]
     [:div.espacados-horizontal
      [botao-consulta-mercado "Consulta Estoque"]
      (when (seq @mercado)
        [button :class "btn-primary" :label "Salva" :on-click #(rf/dispatch [:salva-mercado @mercado])])]
     [gap :size "2em"]
     (when (seq @mercado)
       [tabela-estoque @mercado])]))

(defn view-precos-old []
  (let [historico (rf/subscribe [:historico])
        resposta-historico (rf/subscribe [:resposta-historico])]
    (if (seq @historico)  
      [modal-panel :child [tabela @historico]
       :backdrop-on-click #(rf/dispatch [:limpa-historico])
       :backdrop-opacity "0.5"]
      (if (seq @resposta-historico)
        [:div.centralizado @resposta-historico]
        [:div.espacados-vertical
         [header]
         [titulo "Precos" :level1]
         [feedback]
         [botao-consulta-mercado "Consulta Melhores Precos"]
         [gap :size "2em"]
         (let [mercado (rf/subscribe [:mercado])]
           (when (seq @mercado)
             [:table.table
              [:tbody
               [:tr [:td "Produto"] [:td "Melhor Preco"] [:td "Local"] [:td "Pesquisar"]]
               (for [item (sort-by :nome @mercado)]
                 [:tr
                  [:td (:nome item)] 
                  [:td (formata-preco (:preco item))] 
                  [:td (:local item)]
                  [:td [button :label "+" :class "btn-primary" :on-click #(rf/dispatch [:consulta-historico (:nome item)])]]
                  ])]])) 
         [footer]]))))

(defn view-precos [] 
  (let [produtos (rf/subscribe [:produtos])]
    [:div.espacados-vertical
     [header]
     [feedback]
     [botao-consulta-mercado "Consulta Melhores Precos"]
     [gap :size "2em"]
     [:table.table
      [:tbody
       (for [chave (sort (keys @produtos))]
         [:tr
          [:td chave] 
          [:td (str ((keyword chave) @produtos))]
          [:td [button :label "+" :class "btn-primary" :on-click #(rf/dispatch [:consulta-historico (:nome chave)])]]
          ])]]
     [footer]]))

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
