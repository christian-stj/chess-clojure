(ns chess.core
  (:require
   [cljs.spec.alpha :as s]
   [chess.definitions :as d]
   [chess.helpers :refer [get-board-state get-color-to-move]]
   [chess.rules :refer [in-check? legal-move?]]))

(s/def ::ok string?)
(s/def ::error string?)
(s/def ::game-history ::d/history)
(s/def ::move-result (s/or :success (s/keys :req-un [::ok])
                           :failure (s/keys :req-un [::error])))

(def game-history
  "Vector of move maps in order: {:from [:e :2] :to [:e :4]}."
    (atom [] :validator #(s/valid? ::game-history %)))

(s/fdef get-state
  :ret ::d/board)

(defn get-state []
  (get-board-state @game-history))

(s/fdef get-history
  :ret ::d/annotated-history)

(defn- history-with-check-states [history]
  (mapv (fn [idx move]
          (assoc move :check? (in-check? (subvec history 0 (inc idx)))))
        (range (count history))
        history))

(defn get-history
  ([] (get-history identity))
  ([mapper]
   (mapv mapper (history-with-check-states @game-history))))

(defn check? []
  (in-check? @game-history))

(s/fdef play-move
  :args (s/cat :from ::d/square :to ::d/square :promotion (s/? ::d/promotion))
  :ret  ::move-result)

(defn play-move
  ([from to] (play-move from to nil))
  ([from to promotion]
   (s/assert ::d/square from)
   (s/assert ::d/square to)
   (if (legal-move? @game-history from to promotion)
     (do
       (swap! game-history conj (cond-> {:from from :to to}
                                  promotion (assoc :promotion promotion)))
       {:ok "Move played"})
     {:error "Illegal move"})))

(defn pawn-reaching-last-rank?
  "Returns true if the piece at `from` is a pawn whose move to `to` reaches the promotion rank."
  [from to]
  (let [board (get-board-state @game-history)
        piece (board from)
        [_ to-rank] to]
    (and piece
         (= (:type piece) :pawn)
         (= (:color piece) (get-color-to-move @game-history))
         (or (and (= (:color piece) :white) (= to-rank :8))
             (and (= (:color piece) :black) (= to-rank :1))))))
