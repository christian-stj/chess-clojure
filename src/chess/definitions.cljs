(ns chess.definitions
  (:require [cljs.spec.alpha :as s]))

;; --- Specs ---

(s/def ::color #{:white :black})
(s/def ::piece-type #{:king :queen :rook :bishop :knight :pawn})
(s/def ::file #{:a :b :c :d :e :f :g :h})
(s/def ::rank #{:1 :2 :3 :4 :5 :6 :7 :8})

(s/def ::type ::piece-type)
(s/def ::piece (s/keys :req-un [::type ::color]))

(s/def ::square (s/tuple ::file ::rank))

(s/def ::from ::square)
(s/def ::to ::square)
(s/def ::move (s/keys :req-un [::from ::to]))

(s/def ::history (s/coll-of ::move :kind vector?))

(s/def ::rank-map (s/map-of ::rank ::piece))
(s/def ::board (s/map-of ::square ::piece))

;; --- Data ---

(def initial-board
  {[:a :1] {:type :rook   :color :white} [:a :2] {:type :pawn :color :white} [:a :7] {:type :pawn :color :black} [:a :8] {:type :rook   :color :black}
   [:b :1] {:type :knight :color :white} [:b :2] {:type :pawn :color :white} [:b :7] {:type :pawn :color :black} [:b :8] {:type :knight :color :black}
   [:c :1] {:type :bishop :color :white} [:c :2] {:type :pawn :color :white} [:c :7] {:type :pawn :color :black} [:c :8] {:type :bishop :color :black}
   [:d :1] {:type :queen  :color :white} [:d :2] {:type :pawn :color :white} [:d :7] {:type :pawn :color :black} [:d :8] {:type :queen  :color :black}
   [:e :1] {:type :king   :color :white} [:e :2] {:type :pawn :color :white} [:e :7] {:type :pawn :color :black} [:e :8] {:type :king   :color :black}
   [:f :1] {:type :bishop :color :white} [:f :2] {:type :pawn :color :white} [:f :7] {:type :pawn :color :black} [:f :8] {:type :bishop :color :black}
   [:g :1] {:type :knight :color :white} [:g :2] {:type :pawn :color :white} [:g :7] {:type :pawn :color :black} [:g :8] {:type :knight :color :black}
   [:h :1] {:type :rook   :color :white} [:h :2] {:type :pawn :color :white} [:h :7] {:type :pawn :color :black} [:h :8] {:type :rook   :color :black}})

(def files [:a :b :c :d :e :f :g :h])

(def ranks [:1 :2 :3 :4 :5 :6 :7 :8])
