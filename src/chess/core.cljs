(ns chess.core)

(def initial-board {:a {:1 {:piece :rook :color :white} :2 {:piece :pawn :color :white}  :7 {:piece :pawn :color :black} :8 {:piece :rook :color :black}}
                    :b {:1 {:piece :knight :color :white} :2 {:piece :pawn :color :white}  :7 {:piece :pawn :color :black} :8 {:piece :knight :color :black}}
                    :c {:1 {:piece :bishop :color :white} :2 {:piece :pawn :color :white}  :7 {:piece :pawn :color :black} :8 {:piece :bishop :color :black}}
                    :d {:1 {:piece :queen :color :white} :2 {:piece :pawn :color :white}  :7 {:piece :pawn :color :black} :8 {:piece :queen :color :black}}
                    :e {:1 {:piece :king :color :white} :2 {:piece :pawn :color :white}  :7 {:piece :pawn :color :black} :8 {:piece :king :color :black}}
                    :f {:1 {:piece :bishop :color :white} :2 {:piece :pawn :color :white}  :7 {:piece :pawn :color :black} :8 {:piece :bishop :color :black}}
                    :g {:1 {:piece :knight :color :white} :2 {:piece :pawn :color :white}  :7 {:piece :pawn :color :black} :8 {:piece :knight :color :black}}
                    :h {:1 {:piece :rook :color :white} :2 {:piece :pawn :color :white}  :7 {:piece :pawn :color :black} :8 {:piece :rook :color :black}}})

(def game-history
  "Vector of move maps in order: {:from [:e :2] :to [:e :4]}."
  (atom []))

(defn move-piece [board from to]
  (let [from-file (first from)
        from-rank (second from)
        to-file (first to)
        to-rank (second to)
        piece (get-in board [from-file from-rank])]
    (-> board
        (assoc-in [to-file to-rank] piece)
        (assoc-in [from-file from-rank] nil))))

(defn get-state []
  (reduce (fn [state move] (move-piece state (:from move) (:to move))) initial-board @game-history))

(defn get-history []
  @game-history)

(defn play-move [from to]
  (swap! game-history conj {:from from :to to}))
