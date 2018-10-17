(ns servidor.handler
  (:require [compojure.core :refer :all]
            [clojure.data.json :as json]
            [java-time :as t]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]))

(def produtos (atom [{:produto "banana" :preco 5.55 :local "bistek" :data (str (t/local-date))} {:produto "banana" :preco 2.43 :local "angeloni" :data (str (t/local-date))}]))

(defn filtra-produto [nome] 
  (filter (fn [p] (= nome (:produto p))) @produtos))

(defn consulta [produto]
  (json/write-str (or (filtra-produto produto) {})))

(defn cadastra []
  (json/write-str (or (filtra-produto "banana") {})))

(defroutes app-routes
  (GET "/" [] "Hello World")
  (GET "/consulta/:produto" [produto] (consulta produto))
  (POST "/cadastra" [] (cadastra))
  (route/not-found "Not Found"))

(def app
  (wrap-defaults app-routes site-defaults))
