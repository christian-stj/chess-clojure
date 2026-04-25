(ns chess.core
  (:require
   [cljs.spec.alpha :as s]
   [chess.definitions :as d]
   [chess.helpers :as helpers]
   [chess.rules :as rules]
   [chess.scriptsAndExamples :as scripts]))

(s/def ::game-history ::d/history)

(def game-history
  "Vector of move maps in order: {:from [:e :2] :to [:e :4]}."
  (atom [] :validator #(s/valid? ::game-history %)))

(defn get-state []
  (helpers/get-board-state @game-history))

(defn- history-with-check-states [history]
  (mapv (fn [idx move]
          (assoc move :check? (rules/in-check? (subvec history 0 (inc idx)))))
        (range (count history))
        history))

(defn get-history
  ([] (get-history identity))
  ([mapper]
   (mapv mapper (history-with-check-states @game-history))))

(defn check? []
  (rules/in-check? @game-history))

(defn play-move
  ([from to] (play-move from to nil))
  ([from to promotion]
   (s/assert ::d/square from)
   (s/assert ::d/square to)
   (when promotion (s/assert ::d/promotion promotion))
   (if (rules/legal-move? @game-history from to promotion)
     (do
       (swap! game-history
              conj
              (if promotion
                {:from from :to to :promotion promotion}
                {:from from :to to}))
       {:ok "Move played"})
     {:error "Illegal move"})))

(defn pawn-reaching-last-rank?
  "Returns true if the piece at `from` is a pawn whose move to `to` reaches the promotion rank."
  [from to]
  (s/assert ::d/square from)
  (s/assert ::d/square to)
  (rules/pawn-reaching-last-rank? @game-history from to))
