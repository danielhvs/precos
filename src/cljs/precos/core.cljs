(ns precos.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as reagent :refer [atom]]
            [secretary.core :as secretary :include-macros true]
            [re-frame.core :as rf]
            [clojure.string :as str]
            [cljs-time.format :as f]
            [cljs-time.local :as l]
            [cljs-http.client :as http]
            [cljs.core.async :refer [chan put! <!]]
            [accountant.core :as accountant]))

 ;-- Domino 1 - Event Dispatch -----------------------------------------------

(defn dispatch-timer-event
  []
  (let [now (js/Date.)]
    (rf/dispatch [:timer now])))  ;; <-- dispatch used

;; Call the dispatching function every second.
;; `defonce` is like `def` but it ensures only one instance is ever
;; created in the face of figwheel hot-reloading of this file.
(defonce do-timer (js/setInterval dispatch-timer-event 1000))
;------------------------

(rf/reg-event-db              ;; sets up initial application state
  :initialize                 ;; usage:  (dispatch [:initialize])
  (fn [_ _]                   ;; the two parameters are not important here, so use _
    {:time (js/Date.)         ;; What it returns becomes the new application state
     }))    ;; so the application state will initially be a map with two keys


(rf/reg-event-db                 ;; usage:  (dispatch [:timer a-js-Date])
  :timer                         ;; every second an event of this kind will be dispatched
  (fn [db [_ new-time]]          ;; note how the 2nd parameter is destructured to obtain the data value
    (assoc db :time new-time)))  ;; compute and return the new application state


;; -- Domino 4 - Query  -------------------------------------------------------

(rf/reg-sub
  :time
  (fn [db _]     ;; db is current app state. 2nd unused param is query vector
    (:time db))) ;; return a query computation over the application state

;; -- Domino 5 - View Functions ----------------------------------------------

(defn clock
  []
  [:div.example-clock
   {:style {}}
   (-> @(rf/subscribe [:time])
       .toTimeString
       (str/split " ")
       first)])

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
(defonce servidor "https://infinite-crag-89428.herokuapp.com/")
;(defonce servidor "http://localhost:3000/")


; Feio mas funciona
(def eventos 
  {:debug (fn [p] (prn p))
   :toggle-comprar (fn [{:keys [nome comprar estoque]}]
                     (swap! mercado (fn [a] 
                                      (map (fn [i] (if (first (filter #(= (:nome i) nome) a)) 
                                                     (assoc i :comprar (not (:comprar i)))
                                                     i)) 
                                           a))))
   :update-estoque (fn [{:keys [nome comprar estoque]}]
                     (swap! mercado (fn [a] 
                                      (map (fn [i] (if (first (filter #(= (:nome i) nome) a)) 
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
    (let [p {:nome @cache-produto :preco @cache-preco :local @cache-local} 
          response (<! (try (http/post (operacao "cadastra") {:json-params p :with-credentials? false})
                            (catch :default e
                              (reset! a-debug e))))]
      (consulta)
      (consulta-mercado))))

(defn estilo-compra [p]
  (if (:comprar p) 
    {:text-align "center" :background-color "green" :color "black"}
    {:text-align "center" :background-color "coral" :color "black"}))

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
       (reset! visao @produtos)
       [:div
        #_(when-let [v (first (sort-by :preco @visao))]
          [:table
           [:caption "Mais barato"]
           [:tbody
            (colunas-tabela)
            (elemento v)]])
        (when (not (empty? @visao))
          [:table
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
   [clock]
   [:div [:h2 "Cadastro"]]
   [:div [:label "Produto"] (input-element "p" "p" "input" cache-produto identity) ]
   [:div [:label "Preco"] (input-element "v" "v" "input" cache-preco ->reais)]
   [:div [:label "Local"] (input-element "l" "l" "input" cache-local identity)]
   [:input {:type :button :value "Cadastra" :on-click #(cadastra)}]
   [:div [:label (str "Produtos " @resposta-mercado)]]
   [:div
    (for [p (distinct (map :nome @mercado))] ^{:key (gen-key)}
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

(defn entrada-estoque [p]
  [:div
   [:input {:id "botao"
            :type "button"
            :read-only true
            :value "<-"
            :on-click #(put! canal-eventos [:update-estoque (assoc p :estoque (dec (:estoque p))) :debug p]) }]
   [:label {:style {:padding "12px"}} (:estoque p)]
   [:input {:id "botao"
            :type "button"
            :read-only true
            :value "->"
            :on-click #(put! canal-eventos [:update-estoque (assoc p :estoque (inc (js/parseInt (:estoque p))))]) }]])

(defn elemento-compras [p] ^{:key (gen-key)}
  [:tr 
   [:td {:style (estilo-compra p) 
         :on-click #(put! canal-eventos [:toggle-comprar p])}
    (:nome p)] 
   [:td [entrada-estoque p]]])

(defn tabela-compras []
  (fn []
    [:div
     [:table
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
(consulta-mercado)
