(ns precos-mobile.events
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [re-frame.core :refer [reg-event-db reg-event-fx reg-fx after dispatch]]
   [clojure.spec.alpha :as s]
   [cljs-http.client :as http]
   [goog.string :as gstring]
   [cljs.core.async :refer [<!]]
   [clojure.string :as str]
   [precos-mobile.db :as db :refer [app-db]]))


;; -- Interceptors ------------------------------------------------------------
;; 
;; See https://github.com/Day8/re-frame/blob/master/docs/Interceptors.md
;;
(defn check-and-throw
  "Throw an exception if db doesn't have a valid spec."
  [spec db [event]] 
  (when-not (s/valid? spec db)
    (let [explain-data (s/explain-data spec db)]
      (throw (ex-info (str "Spec check after " event " failed: " explain-data) explain-data)))))

(def validate-spec
  (if goog.DEBUG
    (after (partial check-and-throw ::db/app-db))
    []))

;; -------------------------
;; Funcoes
(def servidor "https://infinite-crag-89428.herokuapp.com/")
#_(def servidor "http://localhost:3000/")

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

;; -- Handlers --------------------------------------------------------------
(defn registra-feedback [db chave valor]
  (assoc db  
    :feedback (assoc (:feedback db) chave valor)))

(reg-event-db
 :initialize-db
 validate-spec
 (fn [_ _]
   app-db))

(reg-event-db
 :set-greeting
 validate-spec
 (fn [db [_ value]]
   (assoc db :greeting value)))

(reg-event-db
 :falha-consulta-mercado
 (fn [db [_ result]]
   (registra-feedback db :resposta-mercado (str "Erro: " (:status-text result)))))

(reg-event-db
 :sucesso-consulta-mercado
 (fn [db [_ result]]
   (assoc (registra-feedback db :resposta-mercado (str result))
     :mercado (filter #(not (nil? (:nome %))) result))))

(reg-event-db
 :falha-consulta-produto
 (fn [db [_ result]]
   (registra-feedback db :resposta-cadastro (str "Erro: " (:status-text result)))))

(reg-event-db
 :sucesso-consulta-produto
 (fn [db [_ result]]
   (assoc
       (registra-feedback db :resposta-cadastro "")
     :nome-consultado (:nome (first result))
     :produtos result)))

(reg-event-db
 :falha-cadastro-produto
 (fn [db [_ result]]
   (registra-feedback db :resposta-cadastro (str "Erro: " (:status-text result)))))

(reg-event-db
 :sucesso-cadastro-produto
 (fn [db [_ result]]
   (registra-feedback db :resposta-cadastro (str "Cadastrado com sucesso " result)
                      )))

(reg-event-fx 
 :consulta-mercado 
 (fn [{:keys [db]} _] 
   {:db (registra-feedback db :resposta-mercado "Consultando lista de mercado...")
    :http-xhrio {:method :get
                 :uri (operacao "consulta-mercado")
                 :timeout 5000
                 :response-format "?"
                 :on-success [:sucesso-consulta-mercado]
                 :on-failure [:falha-consulta-mercado]}} ))

(reg-event-fx 
 :consulta
 (fn [{:keys [db]} [_ nome]] 
   {:db (registra-feedback db :resposta-cadastro (str "Consultando " nome "..."))
    :http-xhrio2 {:method :get
                 :uri (operacao (str "consulta/" nome))
                 :timeout 5000
                 :response-format "?"
                 :on-success [:sucesso-consulta-produto]
                 :on-failure [:falha-consulta-produto]}} ))

(reg-event-fx 
 :cadastra
 (fn [{:keys [db]} [_ p]] 
   {:db (registra-feedback db :resposta-cadastro (str "Cadastrando " p "..."))
    :http-xhrio {:method :post
                 :uri (operacao "cadastra")
                 :params p
                 :timeout 5000
                 :format "?"
                 :response-format "?"
                 :on-success [:sucesso-cadastro-produto]
                 :on-failure [:falha-cadastro-produto]}}))


(reg-event-db
 :falha-salva-mercado
 (fn [db [_ result]]
   (registra-feedback db :resposta-mercado (str "Erro: " (:status-text result)))))

(reg-event-db
 :sucesso-salva-mercado
 (fn [db [_ result]]
   (assoc
       (registra-feedback db :resposta-mercado "")
     :mercado (filter #(not (nil? (:nome %))) (ordena-mercado result)))))

(reg-event-fx 
 :salva-mercado
 (fn [{:keys [db]} [_ mercado]] 
   {:db (registra-feedback db :resposta-mercado "Salvando lista de mercado...")
    :http-xhrio {:method :post
                 :uri (operacao "salva-mercado")
                 :params mercado
                 :timeout 5000
                 :format "?"
                 :response-format "?"
                 :on-success [:sucesso-salva-mercado]
                 :on-failure [:falha-salva-mercado]}}))

(reg-fx
 :http-xhrio2
 (fn [{:keys [on-sucess on-failure]}]
   (dispatch [:falha-consulta-mercado {:status-text "Um erro geraldo"}])))

(reg-fx
 :http-xhrio
 (fn [{:keys [on-sucess on-failure]}]
   (go
    (let [response (<! (http/get (str servidor "consulta-mercado") {:with-credentials? false}))]
      (dispatch [:sucesso-consulta-mercado (:body response)])))
   (dispatch [:sucesso-consulta-mercado [{:nome "arroz" :preco 1.24 :local "bistek"}]])
))

(reg-fx
 :alterar-view
 (fn [{:keys [nova-view db]}]
   (dispatch [:nova-view nova-view])))

(reg-event-fx 
 :altera-view 
 (fn [{:keys [db]} [_ novo]] 
   {:db db
    :alterar-view {:nova-view novo}}))

(reg-event-db                
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
(reg-event-db                
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

(reg-event-db :nova-view (fn [db [_ novo]] (assoc db :view-id novo)))
(reg-event-db :cache-nome (fn [db [_ nova-cache]] (assoc db :cache-nome (normaliza nova-cache))))
(reg-event-db :cache-local (fn [db [_ nova-cache]] (assoc db :cache-local nova-cache)))
(reg-event-db :cache-preco (fn [db [_ nova-cache]] (assoc db :cache-preco
                                                             (if (str/includes? nova-cache ".") 
                                                               nova-cache
                                                               (gstring/format "%.2f" (/ (js/parseInt nova-cache) 100))))))
(reg-event-db :produtos (fn [db [_ novo]] (assoc db :produtos novo)))

(defn consulta-mercado []
  (go
    (let [response (<! (http/get (str servidor "consulta-mercado") {:with-credentials? false}))]
      (dispatch [:sucesso-consulta-mercado (:body response)]))))


