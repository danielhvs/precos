(ns ^:figwheel-no-load precos.dev
  (:require
    [precos.core :as core]
    [devtools.core :as devtools]))

(devtools/install!)

(enable-console-print!)

(core/init!)
