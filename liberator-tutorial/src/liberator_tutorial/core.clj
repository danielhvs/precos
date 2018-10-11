(ns liberator-tutorial.core
  (:require 
   [liberator.core :refer [resource defresource]]
   [liberator.representation :refer [Representation ring-response]]
   [clojure.data.json :as json]
   [ring.middleware.params :refer [wrap-params]]
   [compojure.core :refer [defroutes ANY]]))

(def produtos [{:produto "banana" :preco 2.43 :local "angeloni"}])

(defn filtra-produto [nome] 
  (first (filter (fn [p] (= nome (:produto p))) produtos)))

(defresource preco [produto]

  :allowed-methods [:get]
  :available-media-types ["application/json"]
  :handle-ok (fn [ctx] (ring-response {:headers {"status" "200" "Access-Control-Allow-Origin" "*"} 
                                       :body (json/write-str (filtra-produto produto))})))

(defresource nao-encontrado [p])
  :allowed-methods [:get]
  :available-media-types ["text/html"]
  :handle-ok (fn [ctx] (ring-response {:headers {"status" "200" "Access-Control-Allow-Origin" "*"} 
                                       :body "on-line"}))

(defroutes app
  (ANY "/precos/:produto" [produto] (preco produto))
  (ANY "/*" [p] (nao-encontrado p))

)

(def handler 
  (-> app 
      wrap-params))
