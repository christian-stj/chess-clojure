(ns chess.core
  (:require
   [cljs.spec.alpha :as s]
   [chess.definitions :as d]
   [chess.helpers :refer [get-board-state]]
   [chess.rules :refer [in-check? legal-move?]]))

(s/def ::ok string?)
(s/def ::error string?)
(s/def ::move-result (s/or :success (s/keys :req-un [::ok])
                           :failure (s/keys :req-un [::error])))

(def game-history
  "Vector of move maps in order: {:from [:e :2] :to [:e :4]}."
  (atom []))

(s/fdef get-state
  :ret ::d/board)

(defn get-state []
  (get-board-state @game-history))

(s/fdef get-history
  :ret ::d/history)

(defn get-history []
  @game-history)

(defn check? []
  (in-check? @game-history))

(s/fdef play-move
  :args (s/cat :from ::d/square :to ::d/square)
  :ret  ::move-result)

(defn play-move [from to]
  (s/assert ::d/square from)
  (s/assert ::d/square to)
  (if (legal-move? @game-history from to)
    (do
      (swap! game-history conj {:from from :to to})
      {:ok "Move played"})
    {:error "Illegal move"}))
