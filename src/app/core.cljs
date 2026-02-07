(ns app.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [chess.core :as chess]))

(def pieces
  {:white {:king "♔" :queen "♕" :rook "♖" :bishop "♗" :knight "♘" :pawn "♙"}
   :black {:king "♚" :queen "♛" :rook "♜" :bishop "♝" :knight "♞" :pawn "♟"}})

(def selected-square (r/atom nil))
(def moves-version (r/atom 0))

(defn format-square [[file rank]]
  (str (name file) (name rank)))

(defn move-label [idx move]
  (str (inc idx) ". " (format-square (:from move)) " -> " (format-square (:to move))))

(defn moves-panel []
  (let [_ @moves-version
        moves (chess/get-history)]
    [:div {:style {:text-align "left"}}
     [:h3 {:style {:color "#f0d9b5"
                   :margin "0 0 12px"
                   :font-size "18px"}}
      "Moves"]
     [:div {:style {:max-height "520px"
                    :overflow-y "auto"
                    :padding "8px"
                    :background "#1f1f1f"
                    :border "1px solid #333"
                    :border-radius "6px"}}
      (if (empty? moves)
        [:div {:style {:color "#888" :font-size "14px"}}
         "No moves yet"]
        (for [[idx move] (map-indexed vector moves)]
          ^{:key idx}
          [:div {:style {:color "#ddd"
                         :font-size "14px"
                         :padding "4px 0"
                         :border-bottom "1px solid #2b2b2b"}}
           (move-label idx move)]))]]))

(defn square [row col]
  (let [is-light? (even? (+ row col))
        files [:a :b :c :d :e :f :g :h]
        ranks [8 7 6 5 4 3 2 1]
        file (nth files col)
        rank (keyword (str (nth ranks row)))
        board (chess/get-state)
        piece-data (get-in board [file rank])
        piece-str (when piece-data
              (get-in pieces [(:color piece-data) (:type piece-data)]))
        is-selected? (= @selected-square [file rank])
        bg-color (cond
                   is-selected? "#86a666"
                   is-light? "#f0d9b5"
                   :else "#b58863")
        on-click (fn []
                   (if-let [from @selected-square]
                     (do
                       (chess/play-move from [file rank])
                       (swap! moves-version inc)
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
  [:div {:style {:display "grid"
            :grid-template-columns "1fr 1fr 1fr"
            :gap "24px"
            :align-items "start"
            :margin "0 auto"
            :padding "0 24px"}}
   [:div]
   [:div {:style {:justify-self "center"}}
    [chess-board]]
   [moves-panel]]])

(defn ^:export main []
  (when-let [app (.getElementById js/document "app")]
    (rdom/render [home-page] app)))

(defn ^:export reload []
  (main))
