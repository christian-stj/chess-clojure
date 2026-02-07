(ns app.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [chess.core :as chess]))

(def pieces
  {:white {:king "♔" :queen "♕" :rook "♖" :bishop "♗" :knight "♘" :pawn "♙"}
   :black {:king "♚" :queen "♛" :rook "♜" :bishop "♝" :knight "♞" :pawn "♟"}})

(def selected-square (r/atom nil))

(defn square [row col]
  (let [is-light? (even? (+ row col))
        files [:a :b :c :d :e :f :g :h]
        ranks [8 7 6 5 4 3 2 1]
        file (nth files col)
        rank (keyword (str (nth ranks row)))
        board (chess/get-state)
        piece-data (get-in board [file rank])
        piece-str (when piece-data
                    (get-in pieces [(:color piece-data) (:piece piece-data)]))
        is-selected? (= @selected-square [file rank])
        bg-color (cond
                   is-selected? "#86a666"
                   is-light? "#f0d9b5"
                   :else "#b58863")
        on-click (fn []
                   (if-let [from @selected-square]
                     (do
                       (chess/play-move from [file rank])
                       (reset! selected-square nil))
                     (when piece-data
                       (reset! selected-square [file rank]))))]
    [:div {:on-click on-click
           :style {:width "60px"
                   :height "60px"
                   :background-color bg-color
                   :display "flex"
                   :justify-content "center"
                   :align-items "center"
                   :font-size "40px"
                   :cursor "pointer"
                   :user-select "none"}}
     piece-str]))

(defn chess-board []
  [:div {:style {:display "inline-block"
                 :border "3px solid #333"
                 :background "#2a2a2a"
                 :padding "10px"}}
   [:div {:style {:display "flex"}}
    ;; Rank numbers
    [:div {:style {:display "flex" :flex-direction "column" :margin-right "8px"}}
     (for [n (range 8 0 -1)]
       ^{:key n}
       [:div {:style {:height "60px" :display "flex" :align-items "center"
                      :color "#888" :font-size "14px"}} n])]
    ;; Board
    [:div
     (for [row (range 8)]
       ^{:key row}
       [:div {:style {:display "flex"}}
        (for [col (range 8)]
          ^{:key col}
          [square row col])])]]
   ;; File letters
   [:div {:style {:display "flex" :margin-left "28px" :margin-top "8px"}}
    (for [letter ["a" "b" "c" "d" "e" "f" "g" "h"]]
      ^{:key letter}
      [:div {:style {:width "60px" :text-align "center"
                     :color "#888" :font-size "14px"}} letter])]])

(defn home-page []
  [:div {:style {:text-align "center"}}
   [:h1 {:style {:color "#f0d9b5"
                 :margin-bottom "30px"
                 :font-size "36px"}}
    "Chess"]
   [chess-board]])

(defn ^:export main []
  (when-let [app (.getElementById js/document "app")]
    (rdom/render [home-page] app)))

(defn ^:export reload []
  (main))
