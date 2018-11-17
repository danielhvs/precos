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
            [cljs-http.client :as http]
            [goog.string :as gstring]
            [cljs.core.async :refer [<!]]
            [accountant.core :as accountant]))

(declare salva-mercado)
(declare consulta-mercado)
(declare consulta)
(declare cadastra)
(declare header)
(def servidor "https://infinite-crag-89428.herokuapp.com/")
#_(def servidor "http://localhost:3000/")

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

(defn formata [prefixo p sufixo]
  (str prefixo p sufixo))
(defn formata-aspas [p]
  (formata "'" p "'"))
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
 :falha-consulta-mercado
 (fn [db [_ result]]
   (registra-feedback db :resposta-mercado (str "Erro: " (:status-text result)))))

(rf/reg-event-db
 :sucesso-consulta-mercado
 (fn [db [_ result]]
   (assoc (registra-feedback db :resposta-mercado "")
          :mercado result)))

(rf/reg-event-db
 :falha-consulta-produto
 (fn [db [_ result]]
   (registra-feedback db :resposta-cadastro (str "Erro: " (:status-text result)))))

(rf/reg-event-db
 :sucesso-consulta-produto
 (fn [db [_ result]]
   (assoc
       (registra-feedback db :resposta-cadastro "")
     :nome-consultado (:nome (first result))
     :produtos result)))

(rf/reg-event-db
 :falha-cadastro-produto
 (fn [db [_ result]]
   (registra-feedback db :resposta-cadastro (str "Erro: " (:status-text result)))))

(rf/reg-event-db
 :sucesso-cadastro-produto
 (fn [db [_ result]]
   (registra-feedback db :resposta-cadastro (str "Cadastrado com sucesso " result)
                      )))

(rf/reg-event-fx 
 :consulta-mercado 
 (fn [{:keys [db]} _] 
   {:db (registra-feedback db :resposta-mercado "Consultando lista de mercado...")
    :http-xhrio {:method :get
                 :uri (operacao "consulta-mercado")
                 :timeout 5000
                 :response-format (ajax/json-response-format {:keywords? true})
                 :on-success [:sucesso-consulta-mercado]
                 :on-failure [:falha-consulta-mercado]}} ))

(rf/reg-event-fx 
 :consulta
 (fn [{:keys [db]} [_ nome]] 
   {:db (registra-feedback db :resposta-cadastro (str "Consultando " nome "..."))
    :http-xhrio {:method :get
                 :uri (operacao (str "consulta/" nome))
                 :timeout 5000
                 :response-format (ajax/json-response-format {:keywords? true})
                 :on-success [:sucesso-consulta-produto]
                 :on-failure [:falha-consulta-produto]}} ))

(rf/reg-event-fx 
 :cadastra
 (fn [{:keys [db]} [_ p]] 
   {:db (registra-feedback db :resposta-cadastro (str "Cadastrando " p "..."))
    :http-xhrio {:method :post
                 :uri (operacao "cadastra")
                 :params p
                 :timeout 5000
                 :format (ajax/json-request-format)
                 :response-format (ajax/json-response-format {:keywords? true})
                 :on-success [:sucesso-cadastro-produto]
                 :on-failure [:falha-cadastro-produto]}}))

(rf/reg-event-db
 :falha-salva-mercado
 (fn [db [_ result]]
   (registra-feedback db :resposta-mercado (str "Erro: " (:status-text result)))))

(rf/reg-event-db
 :sucesso-salva-mercado
 (fn [db [_ result]]
   (assoc
    (registra-feedback db :resposta-mercado "")
    :mercado result)))

(rf/reg-event-fx 
 :salva-mercado
 (fn [{:keys [db]} [_ mercado]] 
   {:db (registra-feedback db :resposta-mercado "Salvando lista de mercado...")
    :http-xhrio {:method :post
                 :uri (operacao "salva-mercado")
                 :params mercado
                 :timeout 5000
                 :format (ajax/json-request-format)
                 :response-format (ajax/json-response-format {:keywords? true})
                 :on-success [:sucesso-salva-mercado]
                 :on-failure [:falha-salva-mercado]}}))

(rf/reg-fx
 :alterar-view
 (fn [{:keys [nova-view db]}]
   (secretary/dispatch! nova-view)
   (rf/dispatch [:nova-view nova-view])))

(rf/reg-event-fx 
 :altera-view 
 (fn [{:keys [db]} [_ novo]] 
   {:db db
    :alterar-view {:nova-view novo}}))

(rf/reg-event-db
 :initialize
 (fn [_ _]
   {:view-id "/" :feedback  {}
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
 (fn [db [_ {:keys [nome comprar estoque]} f]] 
   (let [mercado (:mercado db)
         item (first (filter #(= nome (:nome %)) mercado))]
     (assoc db :mercado 
            (map (fn [i] 
                   (if (= item i) 
                     (assoc item :estoque (f (js/parseInt estoque))) 
                     i)) 
                 mercado)))))

(rf/reg-event-db :nova-view (fn [db [_ novo]] (assoc db :view-id novo)))
(rf/reg-event-db :cache-nome (fn [db [_ nova-cache]] (assoc db :cache-nome (normaliza nova-cache))))
(rf/reg-event-db :cache-local (fn [db [_ nova-cache]] (assoc db :cache-local nova-cache)))
(rf/reg-event-db :cache-preco (fn [db [_ nova-cache]] (assoc db :cache-preco
                                                             (if (str/includes? nova-cache ".") 
                                                               nova-cache
                                                               (gstring/format "%.2f" (/ (js/parseInt nova-cache) 100))))))
(rf/reg-event-db :produtos (fn [db [_ novo]] (assoc db :produtos novo)))


;; SUBS
(def subss [:mercado :cache-nome :cache-preco :cache-local :produtos :view-id :nome-consultado :feedback :debug])
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

(defn colunas-tabela []
  [:tr [:td "Produto"] [:td "Preco"] [:td "Data"] [:td "Local"]])

(defn elemento [v] ^{:key (gen-key)}
  [:tr 
   [:td (formata-aspas (:nome v))] 
   [:td (formata-preco (:preco v))] 
   [:td (formata-data (:data v))] 
   [:td (formata-aspas (:local v))]])

(defn tabela []
  (let [visao (rf/subscribe [:produtos])]
    [:div
     (when (not (empty? @visao))
       [:table {:class "table"}
        [:tbody
         (colunas-tabela)
         (for [v @visao] ^{:key (gen-key)}
              (elemento v))]])]))

;; -------------------------
;; Views
(defn botao-consulta-mercado [texto]
  [button :label texto :class "btn-primary" :on-click #(rf/dispatch [:consulta-mercado])])

(defn titulo [t l]
  [title :underline? true :level l :label t])

(defn view-cadastro []
  [:div
   [v-box :children 
    [[:div [titulo "Cadastro" :level1]]
     [:div [input-element :cache-nome :cache-nome "Produto" identity] ]
     [:div [input-com-regex (input-element :cache-preco :cache-preco "Preco" identity) #"^[0-9]*(\.[0-9]{0,2})?$"]]
     [:div [input-element :cache-local :cache-local "Local" identity]]
     [gap :size "2em"]
     [h-box :children
      [[box-centro
        [:div
         [button :class "btn-primary"
                 :label "Cadastra" 
                 :on-click #(rf/dispatch [:cadastra {:nome @(rf/subscribe [:cache-nome])
                                                     :local @(rf/subscribe [:cache-local])
                                                     :preco @(rf/subscribe [:cache-preco])}])]

         [button :label "Consulta" :class "btn-secondary" :on-click #(rf/dispatch [:consulta @(rf/subscribe [:cache-nome])])]
         
         [botao-consulta-mercado "Atualiza Mercado"]
         ]]]]
     [feedback]
     [gap :size "2em"]
     [:div [titulo "Produtos" :level2]]
     [:div
      (for [item @(rf/subscribe [:mercado])] ^{:key (gen-key)}
           [button :style (estilo-compra item) :label (:nome item) :on-click #(rf/dispatch [:cache-nome (:nome item)])])]
     [gap :size "2em"]
     [:div [titulo (str "Locais " @(rf/subscribe [:nome-consultado])) :level2]]
     [:div
      (for [p (distinct (map :local @(rf/subscribe [:produtos])))] ^{:key (gen-key)}
           [button :class "btn-secondary" :label (if (empty? p) "(vazio)!?" p) :on-click #(rf/dispatch [:cache-local p])])]
     [gap :size "2em"]
     [:div [titulo (str "Historico " @(rf/subscribe [:nome-consultado])) :level2]]
     [:div [tabela]]]]
   ]
)

(defn header []
  [horizontal-tabs 
   :model @(rf/subscribe [:view-id])
   :tabs [{:id "/" :label "Precos"} 
          {:id "/estoque" :label "Estoque"} 
          {:id "/cadastro" :label "Cadastro"}]
   :on-change #(rf/dispatch [:altera-view %])])

(defn footer []
  [:div.debug (str "DEBUG: " @(rf/subscribe [:debug]))])

(defn home-page []
  [v-box
   :children [[header]
              [box :child [view-cadastro]]
              [footer]]])

(defn estilo-centro []
  {:text-align "center" :vertical-align "middle"})

(defn colunas-tabela-estoque []
  [:tr 
   [:td {:style (estilo-centro)} "Estoque"]
   [:td {:style (estilo-centro)} "Produto"] 
   ])

(defn entrada-estoque [p]
  [box-centro
   [:div
    [button :class "btn-xs"
     :label "-"
     :on-click #(rf/dispatch [:update-estoque p dec ])]
    [label :style {:padding "2px"} :label (:estoque p)]
    [button :class "btn-xs"
     :label "+"
     :on-click #(rf/dispatch [:update-estoque p inc])]]])

(defn label-mercado [texto]
  [:td {:style (conj (estilo-centro))}
   [:font {:size 2}] texto])


(defn elemento-estoque [p] ^{:key (gen-key)}
  [:tr 
   [:td [entrada-estoque p]]
   [:td {:style (estilo-centro)}
    [:font {:size 2}] (:nome p)]
   #_[:td {:style (conj (estilo-centro) (estilo-compra p)) 
         :on-click #(rf/dispatch [:toggle-comprar p])}
    [:font {:size 2}] (:nome p)] 
   ])

(defn tabela-estoque []
  [:div
   [:table.table
    [:tbody
     [colunas-tabela-estoque]
     (for [p (sort-by :nome @(rf/subscribe [:mercado]))] ^{:key (gen-key)}
          [elemento-estoque p])]]])

(defn estoque []
  [v-box :children [[header] 
                    [titulo "Estoque" :level1]
                    [h-box :children [
                                      [button :class "btn-primary" :label "Salva" :on-click #(rf/dispatch [:salva-mercado @(rf/subscribe [:mercado])])]
                                      [botao-consulta-mercado "Consulta Estoque"]
                                      ]]
                    [feedback]
                    [gap :size "2em"]
                    [tabela-estoque]]]) 

(defn precos []
  [v-box
   :children [[header]
              [botao-consulta-mercado "Consulta Melhores Precos"]
              [feedback]
              (let [mercado (rf/subscribe [:mercado])]
                [:table.table
                 [:tbody
                  [:tr [:td "Produto"] [:td "Melhor Preco"] [:td "Local"]]
                  (for [item (sort-by :nome @mercado)]
                    [:tr [:td (:nome item)] [:td (formata-preco (:preco item))] [:td (:local item)]])]])
              [footer]]])

;; -------------------------
;; Routes

(defonce page (atom #'precos))

(defn current-page []
  [:div [@page]])

(secretary/defroute "/cadastro" []
  (reset! page #'home-page))

(secretary/defroute "/" []
  (reset! page #'precos))

(secretary/defroute "/estoque" []
  (reset! page #'estoque))

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

