(ns chess.core
  (:require
   [chess.moves :refer [is-legal-move? move-piece]]))

(def piece-types #{:king :queen :rook :bishop :knight :pawn})

(def initial-board {:a {:1 {:type :rook :color :white} :2 {:type :pawn :color :white}  :7 {:type :pawn :color :black} :8 {:type :rook :color :black}}
                    :b {:1 {:type :knight :color :white} :2 {:type :pawn :color :white}  :7 {:type :pawn :color :black} :8 {:type :knight :color :black}}
                    :c {:1 {:type :bishop :color :white} :2 {:type :pawn :color :white}  :7 {:type :pawn :color :black} :8 {:type :bishop :color :black}}
                    :d {:1 {:type :queen :color :white} :2 {:type :pawn :color :white}  :7 {:type :pawn :color :black} :8 {:type :queen :color :black}}
                    :e {:1 {:type :king :color :white} :2 {:type :pawn :color :white}  :7 {:type :pawn :color :black} :8 {:type :king :color :black}}
                    :f {:1 {:type :bishop :color :white} :2 {:type :pawn :color :white}  :7 {:type :pawn :color :black} :8 {:type :bishop :color :black}}
                    :g {:1 {:type :knight :color :white} :2 {:type :pawn :color :white}  :7 {:type :pawn :color :black} :8 {:type :knight :color :black}}
                    :h {:1 {:type :rook :color :white} :2 {:type :pawn :color :white}  :7 {:type :pawn :color :black} :8 {:type :rook :color :black}}})

(def game-history
  "Vector of move maps in order: {:from [:e :2] :to [:e :4]}."
  (atom []))

(defn get-state []
  {:post [(every? (fn [piece] (contains? piece-types (:type piece))) (mapcat vals (vals %)))]}
  (reduce (fn [state move] (move-piece state (:from move) (:to move))) initial-board @game-history))

(defn get-history []
  @game-history)

(defn play-move [from to]

  (let [board (get-state)
        color-to-move (if (even? (count @game-history)) :white :black)]
    (if (is-legal-move? board color-to-move from to)
      (do
        (swap! game-history conj {:from from :to to})
        {:ok "Move played"})
      {:error "Illegal move"})))
