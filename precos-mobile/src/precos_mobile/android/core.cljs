(ns precos-mobile.android.core
  (:require [reagent.core :as r :refer [atom]]
            [re-frame.core :refer [subscribe dispatch dispatch-sync]]
            [precos-mobile.events]
            [precos-mobile.subs]))

(def ReactNative (js/require "react-native"))

(def text-input (r/adapt-react-class (.-TextInput ReactNative)))
(def scroll-view (r/adapt-react-class (.-ScrollView ReactNative)))
(def app-registry (.-AppRegistry ReactNative))
(def text (r/adapt-react-class (.-Text ReactNative)))
(def view (r/adapt-react-class (.-View ReactNative)))
(def image (r/adapt-react-class (.-Image ReactNative)))
(def touchable-highlight (r/adapt-react-class (.-TouchableHighlight ReactNative)))

(def logo-img (js/require "./images/cljs.png"))

(defn alert [title]
      (.alert (.-Alert ReactNative) title))

(defn gen-key []
  (gensym "key-"))

(def dados [
            {:nome "arroz" :preco 1.24 :local "bistek"}
            {:nome "feijao" :preco 5.23 :local "outro"}
])

(defn botao-consulta-mercado []
  [touchable-highlight {:style {:background-color "#999" :padding 10 :border-radius 5}
                        :on-press #(dispatch [:consulta-mercado])}
   [text {:style {:color "white" :text-align "center" :font-weight "bold"}} "Consulta Mercado"]]
)

(defn feedback []
  (let [feedback (subscribe [:feedback])]
    (when (not (empty? @feedback))
      [view
       (doall
        (for [f (keys @feedback)] ^{:key (gen-key)}
          [text (f @feedback)]))])))

(defn app-root []
  (let [greeting (subscribe [:get-greeting])]
    (fn []
      [scroll-view
       [view {:style {:flex-direction "column" :margin 40 :align-items "center"}}
        [feedback]
        [botao-consulta-mercado]
        (let [mercado (subscribe [:mercado])]
          (doall 
             (for [item @mercado] ^{:key (gen-key)}
               [view {:style {:flex-direction "row" :margin 10 :align-items "center"}}
                [text (str (:nome item))]
                [text (str (:preco item))]
                [text (str (:local item))]
])))
        [text {:style {:color "white" :text-align "center" :font-weight "bold"}} "Consulta"]
        ]])))

(defn init []
      (dispatch-sync [:initialize-db])
      (.registerComponent app-registry "PrecosMobile" #(r/reactify-component app-root)))


#_[image {:source logo-img
               :style  {:width 80 :height 80 :margin-bottom 30}}]

#_[text {:style {:font-size 30 :font-weight "100" :margin-bottom 20 :text-align "center"}} @greeting]

