(ns liberator-tutorial.core
  (:require 
   [liberator.core :refer [resource defresource]]
   [liberator.representation :refer [Representation ring-response]]
   [clojure.data.json :as json]
   [java-time :as t]
   [ring.middleware.params :refer [wrap-params]]
   [compojure.core :refer [defroutes ANY]]))

(def produtos [{:produto "banana" :preco 5.55 :local "bistek" :data (str (t/local-date))} {:produto "banana" :preco 2.43 :local "angeloni" :data (str (t/local-date))}])

(defn filtra-produto [nome] 
  (filter (fn [p] (= nome (:produto p))) produtos))

(defresource consulta [produto]
  :allowed-methods [:get]
  :available-media-types ["application/json"]
  :handle-ok (fn [ctx] (ring-response {:headers {"status" "200" "Access-Control-Allow-Origin" "*"} 
                                       :body (json/write-str (or (filtra-produto produto) {}))})))
(defroutes app
  (ANY "/consulta/:produto" [produto] (consulta produto))
  (ANY "/*" [] (consulta "nenhum")))

(def handler 
  (-> app 
      wrap-params))
