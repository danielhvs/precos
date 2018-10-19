(defproject servidor "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [compojure "1.6.1"]
                 [ring-cors "0.1.12"]
                 [clojure.java-time "0.3.2"]
                 [org.clojure/data.json "0.2.6"]
                 [ring/ring-jetty-adapter "1.4.0"]
                 [ring/ring-defaults "0.3.2"]]
  :plugins [[environ/environ.lein "0.3.1"][lein-ring "0.12.4"]]
  :hooks [environ.leiningen.hooks]
  :uberjar-name "servidor-0.1.0-SNAPSHOT-standalone.jar"
  :ring {:handler servidor.handler/app}
  :profiles
  {:production {:env {:production true}} 
   :dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring/ring-mock "0.3.2"]]}})



