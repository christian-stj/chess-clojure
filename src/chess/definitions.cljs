(ns chess.definitions)

(def initial-board {:a {:1 {:type :rook :color :white} :2 {:type :pawn :color :white}  :7 {:type :pawn :color :black} :8 {:type :rook :color :black}}
                    :b {:1 {:type :knight :color :white} :2 {:type :pawn :color :white}  :7 {:type :pawn :color :black} :8 {:type :knight :color :black}}
                    :c {:1 {:type :bishop :color :white} :2 {:type :pawn :color :white}  :7 {:type :pawn :color :black} :8 {:type :bishop :color :black}}
                    :d {:1 {:type :queen :color :white} :2 {:type :pawn :color :white}  :7 {:type :pawn :color :black} :8 {:type :queen :color :black}}
                    :e {:1 {:type :king :color :white} :2 {:type :pawn :color :white}  :7 {:type :pawn :color :black} :8 {:type :king :color :black}}
                    :f {:1 {:type :bishop :color :white} :2 {:type :pawn :color :white}  :7 {:type :pawn :color :black} :8 {:type :bishop :color :black}}
                    :g {:1 {:type :knight :color :white} :2 {:type :pawn :color :white}  :7 {:type :pawn :color :black} :8 {:type :knight :color :black}}
                    :h {:1 {:type :rook :color :white} :2 {:type :pawn :color :white}  :7 {:type :pawn :color :black} :8 {:type :rook :color :black}}})

(def piece-types #{:king :queen :rook :bishop :knight :pawn})

(def files [:a :b :c :d :e :f :g :h])

(def ranks [:1 :2 :3 :4 :5 :6 :7 :8])
