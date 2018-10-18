(ns servidor.handler
  (:require [compojure.core :refer :all]
            [clojure.data.json :as json]
            [java-time :as t]
            [compojure.route :as route]
            [ring.util.response :as r]
            [ring.middleware.cors :refer [wrap-cors]]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]))

(def produtos (atom [{:produto "banana" :preco 5.55 :local "bistek" :data (str (t/local-date))} {:produto "banana" :preco 2.43 :local "angeloni" :data (str (t/local-date))}]))
(def mercado (atom #{{:produto "banana" :comprar true}}))

(defn filtra-produto [nome colecao] 
  (filter (fn [p] (= nome (:produto p))) colecao))

(defn consulta-mercado []
  (-> (r/response (json/write-str @mercado))
      (r/header  "Access-Control-Allow-Origin" "*")))

(defn consulta [produto]
  (-> (r/response (json/write-str (or (filtra-produto produto @produtos) {})))
      (r/header "Access-Control-Allow-Origin" "*")))

(defn cadastra [request]
  (-> (r/response
       (dosync (let [p (json/read-str (slurp (:body request)) :key-fn keyword)]
                 (swap! produtos conj p)
                 (when-not (some #(= (:produto p) %) (map :produto @mercado)) (swap! mercado conj {:produto (:produto p) :comprar true}))
                 (json/write-str (or (filtra-produto (:produto p) @produtos) {})))))
      
      (r/header "Access-Control-Allow-Origin" "*")))

(defn opcoes []
  (-> (r/response "")
      (r/header "Access-Control-Allow-Origin" "*")
      (r/header "Allow" "POST")
      (r/header "Access-Control-Allow-Methods" "POST")
      (r/header "Access-Control-Allow-Headers" "content-type")))

(defroutes app-routes
  (GET "/" [] "Hello World")
  (GET "/consulta/:produto" [produto] (consulta produto))
  (GET "/consulta-mercado" [] (consulta-mercado))
  (POST "/cadastra" request (cadastra request))
  (OPTIONS "/cadastra" request (opcoes))
  (route/not-found "Not Found"))

(defn wrap-debug [handler]
  (fn [request]
    (do
      (println "REQUEST: " request)
      (let [response (handler request)]
        (do (println "RESPONSE: " response)
            response))
)))

(def app
  (wrap-debug
   (wrap-cors 
    (wrap-defaults app-routes api-defaults) 
    :access-control-allow-origin [#".*"])))
