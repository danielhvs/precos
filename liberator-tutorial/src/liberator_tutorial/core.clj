(ns liberator-tutorial.core
  (:require [liberator.core :refer [resource defresource]]
            [ring.middleware.params :refer [wrap-params]]
            [compojure.core :refer [defroutes ANY]]))

(defresource preco [produto]
  :available-media-types ["text/plain"]
  :handle-ok (fn [_] (format "Consultar produto %s" produto)))

(defroutes app
  (ANY "/precos/:produto" [produto] (preco produto)))

(def handler 
  (-> app 
      wrap-params))
