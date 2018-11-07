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
(defn ordena-mercado [mercado]
  (sort-by (juxt :estoque :nome) mercado))


;; EVENTS
(rf/reg-event-db
  :falha-consulta-mercado
  (fn [db [_ result]]
    (assoc db :resposta-mercado (str "Erro: " (:status-text result)))))

(rf/reg-event-db
  :sucesso-consulta-mercado
  (fn [db [_ result]]
    (assoc db
      :resposta-mercado ""
      :mercado (filter #(not (nil? (:nome %))) result))))

(rf/reg-event-db
  :falha-consulta-produto
  (fn [db [_ result]]
    (assoc db :resposta-cadastro (str "Erro: " (:status-text result)))))

(rf/reg-event-db
  :sucesso-consulta-produto
  (fn [db [_ result]]
    (assoc db
      :nome-consultado (:nome (first result))
      :resposta-cadastro ""
      :produtos result)))

(rf/reg-event-db
  :falha-cadastro-produto
  (fn [db [_ result]]
    (assoc db :resposta-cadastro (str "Erro: " (:status-text result)))))

(rf/reg-event-db
  :sucesso-cadastro-produto
  (fn [db [_ result]]
    (assoc db :resposta-cadastro (str "Cadastrado com sucesso " result))))

(rf/reg-event-fx 
 :consulta-mercado 
 (fn [{:keys [db]} _] 
  {:db (assoc db :resposta-mercado "Consultando lista de mercado...")
   :http-xhrio {:method :get
                :uri (operacao "consulta-mercado")
                :timeout 5000
                :response-format (ajax/json-response-format {:keywords? true})
                :on-success [:sucesso-consulta-mercado]
                :on-failure [:falha-consulta-mercado]}} ))

(rf/reg-event-fx 
 :consulta
 (fn [{:keys [db]} [_ nome]] 
  {:db (assoc db :resposta-cadastro (str "Consultando " nome "..."))
   :http-xhrio {:method :get
                :uri (operacao (str "consulta/" nome))
                :timeout 5000
                :response-format (ajax/json-response-format {:keywords? true})
                :on-success [:sucesso-consulta-produto]
                :on-failure [:falha-consulta-produto]}} ))

(rf/reg-event-fx 
 :cadastra
 (fn [{:keys [db]} [_ p]] 
  {:db (assoc db :resposta-cadastro (str "Cadastrando " p "..."))
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
    (assoc db :resposta-mercado (str "Erro: " (:status-text result)))))

(rf/reg-event-db
  :sucesso-salva-mercado
  (fn [db [_ result]]
    (assoc db :resposta-mercado ""
           :mercado (filter #(not (nil? (:nome %))) (ordena-mercado result)))))

(rf/reg-event-fx 
 :salva-mercado
 (fn [{:keys [db]} [_ mercado]] 
  {:db (assoc db :resposta-mercado "Salvando lista de mercado...")
   :http-xhrio {:method :post
                :uri (operacao "salva-mercado")
                :params mercado
                :timeout 5000
                :format (ajax/json-request-format)
                :response-format (ajax/json-response-format {:keywords? true})
                :on-success [:sucesso-salva-mercado]
                :on-failure [:falha-salva-mercado]}}))

(rf/reg-event-db
  :initialize
  (fn [_ _]
    {:view-id "/"
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
  (fn [db [_ {:keys [nome comprar estoque]}]] 
    (let [mercado (:mercado db)
          item (first (filter #(= nome (:nome %)) mercado))]
      (assoc db :mercado 
             (map (fn [i] 
                    (if (= item i) 
                      (assoc item :estoque estoque) 
                      i)) 
                  mercado)))))

(rf/reg-event-db :resposta-mercado (fn [db [_ nova-resposta]] (assoc db :resposta-mercado nova-resposta)))
(rf/reg-event-db :resposta-cadastro (fn [db [_ nova-resposta]] (assoc db :resposta-cadastro nova-resposta)))
(rf/reg-event-db :cache-nome (fn [db [_ nova-cache]] (assoc db :cache-nome (normaliza nova-cache))))
(rf/reg-event-db :cache-local (fn [db [_ nova-cache]] (assoc db :cache-local nova-cache)))
(rf/reg-event-db :cache-preco (fn [db [_ nova-cache]] (assoc db :cache-preco
                                                             (if (str/includes? nova-cache ".") 
                                                               nova-cache
                                                               (gstring/format "%.2f" (/ (js/parseInt nova-cache) 100))))))
(rf/reg-event-db :produtos (fn [db [_ novo]] (assoc db :produtos novo)))
(rf/reg-event-db :altera-view (fn [db [_ novo]] 
                                (secretary/dispatch! novo)
                                (assoc db :view-id novo)))

;; SUBS
(rf/reg-sub :mercado (fn [db _] (:mercado db)))
(rf/reg-sub :resposta-mercado (fn [db _] (:resposta-mercado db)))
(rf/reg-sub :resposta-cadastro (fn [db _] (:resposta-cadastro db)))
(rf/reg-sub :cache-nome (fn [db _] (:cache-nome db)))
(rf/reg-sub :cache-preco (fn [db _] (:cache-preco db)))
(rf/reg-sub :cache-local (fn [db _] (:cache-local db)))
(rf/reg-sub :produtos (fn [db _] (:produtos db)))
(rf/reg-sub :view-id (fn [db _] (:view-id db)))
(rf/reg-sub :nome-consultado (fn [db _] (:nome-consultado db)))
(rf/reg-sub :debug (fn [db _] (:debug db)))

;; VIEW
(defn gen-key []
  (gensym "key-"))

;; -------------------------
;; Componentes
(defn box-centro [componente]
  [box :align :center :justify :around :child componente])

(defn feedback [resposta]
  (if (not (str/blank? resposta)) 
    [h-box :children [
                      [:div [throbber]]
                      (box-centro [:div [:label resposta]])]]
    [:div]))

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
        [:div [button :class "btn-primary"
               :label "Cadastra" 
               :on-click #(rf/dispatch [:cadastra {:nome @(rf/subscribe [:cache-nome])
                                                   :local @(rf/subscribe [:cache-local])
                                                   :preco @(rf/subscribe [:cache-preco])}])]]]
       [feedback @(rf/subscribe [:resposta-cadastro])]
       [feedback @(rf/subscribe [:resposta-mercado])]
       ]]
     [gap :size "1em"]
     [button :label "Consulta" :class "btn-secondary" :on-click #(rf/dispatch [:consulta @(rf/subscribe [:cache-nome])])]
     [button :label "Consulta Mercado" :class "btn-secondary" :on-click #(rf/dispatch [:consulta-mercado])]
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
     [:div [titulo (str "Historico " @(rf/subscribe [:nome-consultado])) :level2]]
     [:div [tabela]]]]
   ]
)

(defn header []
  [horizontal-tabs 
   :model @(rf/subscribe [:view-id])
   :tabs [{:id "/lista-compras" :label "Lista de Compras"} {:id "/" :label "Cadastro"}]
   :on-change #(rf/dispatch [:altera-view %])])

(defn footer []
  [:div (str @(rf/subscribe [:debug]))])

(defn home-page []
  [v-box
   :children [[header]
              [box :child [view-cadastro]]
              [footer]]])

(defn estilo-centro []
  {:text-align "center" :vertical-align "middle"})

(defn colunas-tabela-compras []
  [:tr 
   [:td {:style (estilo-centro)} "Estoque"]
   [:td {:style (estilo-centro)} "Produto"] 
   [:td {:style (estilo-centro)} "Preco"] 
   [:td {:style (estilo-centro)} "Local"] 
   ])

(defn entrada-estoque [p]
  (box-centro
   [:div
    [button :class "btn-xs"
     :label "-"
     :on-click #(rf/dispatch [:update-estoque (assoc p :estoque (dec (js/parseInt (:estoque p))))])]
    [label :style {:padding "2px"} :label (:estoque p)]
    [button :class "btn-xs"
     :label "+"
     :on-click #(rf/dispatch [:update-estoque (assoc p :estoque (inc (js/parseInt (:estoque p))))]) ]]))

(defn label-mercado [texto]
  [:td {:style (conj (estilo-centro))}
   [:font {:size 2}] texto])

(defn elemento-compras [p] ^{:key (gen-key)}
  [:tr 
   [:td [entrada-estoque p]]
   [:td {:style (conj (estilo-centro) (estilo-compra p)) 
         :on-click #(rf/dispatch [:toggle-comprar p])}
    [:font {:size 2}] (:nome p)] 
   (label-mercado (formata-preco (:preco p)))
   (label-mercado (:local p))
   ])

(defn tabela-compras []
  [:div
   [:table {:class "table"}
    [:tbody
     [colunas-tabela-compras]
     (for [p @(rf/subscribe [:mercado])] ^{:key (gen-key)}
          [elemento-compras p])]]])

(defn lista-compras []
  [v-box :children [[header] 
                    [titulo "Lista de Compras" :level1]
                    [h-box :children [[button :class "btn-primary" :label "Salva" :on-click #(rf/dispatch [:salva-mercado @(rf/subscribe [:mercado])])]
                                      (feedback @(rf/subscribe [:resposta-mercado]))]]
                    [gap :size "2em"]
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

