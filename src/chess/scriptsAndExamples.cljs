(ns chess.scriptsAndExamples)

;; Reset the game to start fresh
;; (reset! chess/game-history [])

;; ---------------------------------------------------------------------------
;; The Opera Game — Paul Morphy vs. Duke of Brunswick & Count Isouard
;; Paris, 1858
;;
;; Famous for: open play, a rook sacrifice, a queen sacrifice, and a
;; back-rank checkmate delivered in 17 moves.
;;
;; Paste moves one by one in the REPL, or call (play-opera-game) to replay
;; the whole game at once.
;; ---------------------------------------------------------------------------

(def opera-game-moves
  [;; 1. e4 e5
   [[:e :2] [:e :4]]
   [[:e :7] [:e :5]]
   ;; 2. Nf3 d6
   [[:g :1] [:f :3]]
   [[:d :7] [:d :6]]
   ;; 3. d4 Bg4
   [[:d :2] [:d :4]]
   [[:c :8] [:g :4]]
   ;; 4. dxe5 Bxf3  — White gambits the center, Black pins the knight
   [[:d :4] [:e :5]]
   [[:g :4] [:f :3]]
   ;; 5. Qxf3 dxe5
   [[:d :1] [:f :3]]
   [[:d :6] [:e :5]]
   ;; 6. Bc4 Nf6
   [[:f :1] [:c :4]]
   [[:g :8] [:f :6]]
   ;; 7. Qb3  — Attacks f7 and b7, Black scrambles
   [[:f :3] [:b :3]]
   [[:d :8] [:e :7]]
   ;; 8. Nc3 c6
   [[:b :1] [:c :3]]
   [[:c :7] [:c :6]]
   ;; 9. Bg5 b5  — Black tries to drive the bishop away
   [[:c :1] [:g :5]]
   [[:b :7] [:b :5]]
   ;; 10. Nxb5! cxb5  — Morphy sacrifices the knight
   [[:c :3] [:b :5]]
   [[:c :6] [:b :5]]
   ;; 11. Bxb5+ Nbd7  — Check! Knight blocks
   [[:c :4] [:b :5]]
   [[:b :8] [:d :7]]
   ;; 12. O-O-O Rd8  — Morphy castles queenside, Black develops the rook
   [[:e :1] [:c :1]]
   [[:a :8] [:d :8]]
   ;; 13. Rxd7! Rxd7  — Rook sacrifice to shatter Black's defense
   [[:d :1] [:d :7]]
   [[:d :8] [:d :7]]
   ;; 14. Rd1 Qe6  — The second rook joins the attack
   [[:h :1] [:d :1]]
   [[:e :7] [:e :6]]
   ;; 15. Bxd7+ Nxd7
   [[:b :5] [:d :7]]
   [[:f :6] [:d :7]]
   ;; 16. Qb8+!! Nxb8  — Queen sacrifice!
   [[:b :3] [:b :8]]
   [[:d :7] [:b :8]]
   ;; 17. Rd8#  — Checkmate
   [[:d :1] [:d :8]]])

(def Christian
  {:name "Christian Stjernberg"
   :roll "Fullstackutvecklare"
   :kund "Swedbank"
   :facts ["Gillar programspråk & ledarskaps-/teamfrågor"
           "Aktiv i olika nationella scoutprojekt"
           "Spelar diverse instrument (främst piano)"]})

(Christian)

(comment  ; For presentation

  (def initial-board
    {[:a :2] {:type :pawn :color :white} [:a :1] {:type :rook   :color :white}
     [:b :2] {:type :pawn :color :white} [:b :1] {:type :knight :color :white}
     [:c :2] {:type :pawn :color :white} [:c :1] {:type :bishop :color :white}
     [:d :2] {:type :pawn :color :white} [:d :1] {:type :queen  :color :white}
     [:e :2] {:type :pawn :color :white} [:e :1] {:type :king   :color :white}
     [:f :2] {:type :pawn :color :white} [:f :1] {:type :bishop :color :white}
     [:g :2] {:type :pawn :color :white} [:g :1] {:type :knight :color :white}
     [:h :2] {:type :pawn :color :white} [:h :1] {:type :rook   :color :white}

     [:a :7] {:type :pawn :color :black} [:a :8] {:type :rook   :color :black}
     [:b :7] {:type :pawn :color :black} [:b :8] {:type :knight :color :black}
     [:c :7] {:type :pawn :color :black} [:c :8] {:type :bishop :color :black}
     [:d :7] {:type :pawn :color :black} [:d :8] {:type :queen  :color :black}
     [:e :7] {:type :pawn :color :black} [:e :8] {:type :king   :color :black}
     [:f :7] {:type :pawn :color :black} [:f :8] {:type :bishop :color :black}
     [:g :7] {:type :pawn :color :black} [:g :8] {:type :knight :color :black}
     [:h :7] {:type :pawn :color :black} [:h :8] {:type :rook   :color :black}})

  game-history
  [{:from [:d :2], :to [:d :4]}
   {:from [:d :7], :to [:d :5]}]

  initial-board


  (defn play-move
    [game-history from to]
    (if (legal-move? game-history from to)
      (conj game-history {:from from :to to})
      game-history))


  (defn get-board-state [game-history]
    (reduce (fn [board-state {:keys [from to]}]
              (move-piece board-state from to))
            initial-board
            game-history))


  (defn move-piece
    [board from to]
    (let [piece (board from)]
      (-> board
          (assoc to piece)
          (dissoc from))))


  get-board-state
  )


(comment ; For demo

  ; Force move piece
  (swap! game-history conj {:from [:a :8] :to [:a :1]})

  ; Remove last n moves
  (dotimes [_ 3]
    (swap! game-history pop))

  ; Who is to play?
  (if (even? (count @game-history)) :white :black)

  ; How many moves have we done?
  (count @game-history)

  ; Get current board
  (get-state)

  ; Get piece type at position
  (:type ((get-state) [:a :1]))

  ; Get history with check states
  (get-history)

  ; Replay opera game
  (doseq [[from to] (take 28 scripts/opera-game-moves)]
    (play-move from to))

  (rules/legal-move? @game-history [:d :7] [:c :5])


  ; Paredit example - good example for paredit navigation and editing that shows usage of various paredit commands
  (def example
    (let [x 1
          y 2
          z 3]
      (* (+ x y) z)))

  (def invert {false true true false})

  )
