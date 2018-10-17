(ns servidor.handler
  (:require [compojure.core :refer :all]
            [clojure.data.json :as json]
            [java-time :as t]
            [compojure.route :as route]
            [ring.util.response :as r]
            [ring.middleware.cors :refer [wrap-cors]]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]))

(def produtos (atom [{:produto "banana" :preco 5.55 :local "bistek" :data (str (t/local-date))} {:produto "banana" :preco 2.43 :local "angeloni" :data (str (t/local-date))}]))

(defn filtra-produto [nome] 
  (filter (fn [p] (= nome (:produto p))) @produtos))

(defn consulta [produto]
  (r/header
   (r/response
    (json/write-str (or (filtra-produto produto) {})))
   "Access-Control-Allow-Origin" "*"))

(defn cadastra [request]
  (r/header
   (r/response
    (dosync (let [p (json/read-str (slurp (:body request)) :key-fn keyword)]
              (swap! produtos conj p)
              (json/write-str (or (filtra-produto (:produto p)) {})))))
   "Access-Control-Allow-Origin" "*"
   ))

(defn opcoes []
  (r/header 
   (r/header
    (r/header
     (r/header (r/response "") "Access-Control-Allow-Origin" "*")
     "Allow" "POST")
    "Access-Control-Allow-Methods" "POST")
   "Access-Control-Allow-Headers" "content-type"))

(defroutes app-routes
  (GET "/" [] "Hello World")
  (GET "/consulta/:produto" [produto] (consulta produto))
  (POST "/cadastra" request (do (println request) (cadastra request)))
  (OPTIONS "/cadastra" request (do (println request) (opcoes)))
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
