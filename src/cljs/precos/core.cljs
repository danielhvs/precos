(ns precos.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as reagent :refer [atom]]
            [secretary.core :as secretary :include-macros true]
            [cljs-time.format :as f]
            [cljs-time.local :as l]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
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

(defn salva-mercado []
  (go
    (let [response (<! (try (http/post "http://localhost:3000/salva-mercado" {:json-params @mercado :with-credentials? false})
                            (catch :default e
                              (reset! a-debug e))))]
      (reset! resposta (:body response)))))

(defn consulta-mercado []
  (go
    (let [response (<! (try (http/get "http://localhost:3000/consulta-mercado" {:with-credentials? false})
                            (catch :default e
                              (reset! a-debug e))))]
      (reset! resposta "Atualizado")
      (reset! mercado (reverse (sort-by :comprar (json->clj (:body response))))))))

(defn cadastra [] 
  (go 
    (let [p {:produto @cache-produto :preco @cache-preco :local @cache-local} 
          response (<! (try (http/post "http://10.107.7.69:3000/cadastra" {:json-params p :with-credentials? false})
                            (catch :default e
                              (reset! a-debug e))))]
      (consulta-mercado)
      (reset! produtos (json->clj (:body response))))))

(defn consulta []
  (go
    (let [response (<! (try (http/get (str "http://localhost:3000/consulta/" @cache-produto) {:with-credentials? false})
                            (catch :default e
                              (reset! a-debug e))))]
      (reset! produtos (json->clj (:body response))))))

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
  (let [visao (atom []) ]
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
   [:div
    (for [p (distinct (map :produto @mercado))] ^{:key (gen-key)}
         [:input {:type :button :value p :on-click #(do 
                                                      (reset! cache-produto p)
                                                      (consulta))}])]
   [:div
    (for [p (distinct (map :local @produtos))] ^{:key (gen-key)}
         [:input {:type :button :value p :on-click #(reset! cache-local p)}])]
   [:div [tabela]]
   [:div [debug]]
   ])

(defn lista-compras []
  [:div [:a {:href "/"} "Preços dos produtos"]
   [:div 
    [:h2 "Lista de Compras"]
    [:input {:type :button :value "Atualiza" :on-click #(consulta-mercado)}]
    [:input {:type :button :value "Salva" :on-click #(salva-mercado)}]
    [:label @resposta]
    (for [p @mercado] ^{:key (gen-key)}
         [:div [:input {:style (estilo-botao p) :type :button :value (:produto p) 
                        :on-click #(toggle-comprar p)}]])]])

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
