(ns chess.core
  (:require
   [cljs.spec.alpha :as s]
   [chess.definitions :as d]
   [chess.helpers :refer [get-board-state]]
   [chess.rules :refer [in-check? legal-move?]]))

(def game-history
  "Vector of move maps in order: {:from [:e :2] :to [:e :4]}."
  (atom []))

(defn get-state []
  {:post [(s/valid? ::d/board %)]}
  (get-board-state @game-history))

(defn get-history []
  {:post [(s/valid? ::d/history %)]}
  @game-history)

(defn check? []
  (in-check? @game-history))

(defn play-move [from to]
  {:pre [(s/valid? ::d/square from)
         (s/valid? ::d/square to)]}
  (if (legal-move? @game-history from to)
    (do
      (swap! game-history conj {:from from :to to})
      {:ok "Move played"})
    {:error "Illegal move"}))
