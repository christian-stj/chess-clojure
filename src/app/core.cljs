(ns app.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [chess.core :as chess]))

(def pieces
  {:white {:king "♔" :queen "♕" :rook "♖" :bishop "♗" :knight "♘" :pawn "♙"}
   :black {:king "♚" :queen "♛" :rook "♜" :bishop "♝" :knight "♞" :pawn "♟"}})

(def selected-square (r/atom nil))
(def pending-promotion (r/atom nil))
(def ui-version (r/atom 0))

(defn format-square [[file rank]]
  (str (name file) (name rank)))

(def promotion-label
  {:queen "Q"
   :rook "R"
   :bishop "B"
   :knight "N"})

(defn move-label [idx move]
  (str (inc idx)
       ". "
       (format-square (:from move))
       " -> "
       (format-square (:to move))
       (when-let [promotion (:promotion move)]
         (str "=" (get promotion-label promotion (name promotion))))
       (when (:check? move) "+")))

(defn moves-panel [moves]
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
         (move-label idx move)]))]])

(defn square [board row col]
  (let [light? (even? (+ row col))
        files [:a :b :c :d :e :f :g :h]
        ranks [8 7 6 5 4 3 2 1]
        file (nth files col)
        rank (keyword (str (nth ranks row)))
        piece-data (board [file rank])
        piece-str (when piece-data
              (get-in pieces [(:color piece-data) (:type piece-data)]))
        selected? (= @selected-square [file rank])
        bg-color (cond
                   selected? "#86a666"
                   light? "#f0d9b5"
                   :else "#b58863")
        on-click (fn []
                   (if-let [from @selected-square]
                     (do
                       (if (chess/pawn-reaching-last-rank? from [file rank])
                         (reset! pending-promotion {:from from :to [file rank]})
                         (when (:ok (chess/play-move from [file rank]))
                           (swap! ui-version inc)))
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

(defn promotion-picker [board]
  (when-let [{:keys [from to]} @pending-promotion]
    (let [color (:color (board from))
          options [:queen :rook :bishop :knight]]
      [:div {:style {:position "fixed"
                     :top 0 :left 0 :right 0 :bottom 0
                     :background "rgba(0,0,0,0.6)"
                     :display "flex"
                     :justify-content "center"
                     :align-items "center"
                     :z-index 100}}
       [:div {:style {:background "#2a2a2a"
                      :border "2px solid #f0d9b5"
                      :border-radius "12px"
                      :padding "20px 24px"
                      :text-align "center"}}
        [:div {:style {:color "#f0d9b5"
                       :font-size "16px"
                       :margin-bottom "16px"}}
         "Promote to:"]
        [:div {:style {:display "flex" :gap "12px"}}
         (for [piece-type options]
           ^{:key piece-type}
           [:div {:on-click (fn []
                              (when (:ok (chess/play-move from to piece-type))
                                (swap! ui-version inc)
                                (reset! pending-promotion nil)))
                  :style {:width "60px"
                          :height "60px"
                          :background "#b58863"
                          :border-radius "8px"
                          :display "flex"
                          :justify-content "center"
                          :align-items "center"
                          :font-size "40px"
                          :cursor "pointer"
                          :user-select "none"}}
            (get-in pieces [color piece-type])])]]])))

(defn chess-board [board check?]
  [:div {:style {:display "inline-block"
                 :border (if check? "3px solid #ff3b3b" "3px solid #333")
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
          [square board row col])])]]
   ;; File letters
   [:div {:style {:display "flex" :margin-left "28px" :margin-top "8px"}}
    (for [letter ["a" "b" "c" "d" "e" "f" "g" "h"]]
      ^{:key letter}
      [:div {:style {:width "60px" :text-align "center"
                     :color "#888" :font-size "14px"}} letter])]])

(defn toolbar []
  [:div {:style {:position "absolute"
                 :top "16px"
                 :right "16px"
                 :display "flex"
                 :gap "10px"
                 :z-index 20}}
   [:button {:on-click (fn []
                         (reset! selected-square nil)
                         (reset! pending-promotion nil)
                         (swap! ui-version inc))
             :style {:background "#2f2f2f"
                     :color "#f0d9b5"
                     :border "1px solid #555"
                     :border-radius "8px"
                     :padding "8px 14px"
                     :font-size "14px"
                     :font-weight "600"
                     :cursor "pointer"}}
    "Refresh"]])

(defn home-page []
  (let [_ @ui-version
        board (chess/get-board)
        moves (chess/get-history)
        check? (chess/check?)]
    [:div {:style {:text-align "center"
                   :position "relative"}}
     [toolbar]
     [:h1 {:style {:color "#f0d9b5"
                   :margin-bottom "30px"
                   :font-size "36px"}}
      "Chess"]
     [promotion-picker board]
     [:div {:style {:display "grid"
                    :grid-template-columns "1fr 1fr 1fr"
                    :gap "24px"
                    :align-items "start"
                    :margin "0 auto"
                    :padding "0 24px"}}
      ;; Left column: Check indicator
      [:div {:style {:display "flex"
                     :justify-content "flex-end"
                     :padding-right "8px"}}
       (when check?
         [:div {:style {:color "#ff3b3b"
                        :font-weight "700"
                        :font-size "18px"}}
          "Check!"])]
      ;; Middle column: Board
      [:div {:style {:justify-self "center"}}
       [chess-board board check?]]
      ;; Right column: Moves
      [moves-panel moves]]]))

(defn ^:export main []
  (when-let [app (.getElementById js/document "app")]
    (rdom/render [home-page] app)))

(defn ^:export reload []
  (main))
