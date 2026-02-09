(ns chess.core
  (:require
   [chess.definitions :refer [files piece-types ranks]]
   [chess.helpers :refer [get-board-state]]
   [chess.rules :refer [is-check? is-legal-move?]]))

(def game-history
  "Vector of move maps in order: {:from [:e :2] :to [:e :4]}."
  (atom []))

(defn get-state []
  {:post [(every? (fn [piece] (contains? piece-types (:type piece))) (mapcat vals (vals %)))]}
  (get-board-state @game-history))

(defn get-history []
  {:post [(every? (fn [move] (and (contains? move :from) (contains? move :to))) %)
          (every? (fn [move] (and (contains? (set files) (first (:from move))) (contains? (set ranks) (second (:from move))))) %)
          (every? (fn [move] (and (contains? (set files) (first (:to move))) (contains? (set ranks) (second (:to move))))) %)]}
  @game-history)

(defn get-is-check []
  (is-check? @game-history))

(defn play-move [from to]
  {:pre [(contains? (set files) (first from))
         (contains? (set ranks) (second from))
         (contains? (set files) (first to))
         (contains? (set ranks) (second to))]}
  (if (is-legal-move? @game-history from to)
    (do
      (swap! game-history conj {:from from :to to})
      {:ok "Move played"})
    {:error "Illegal move"}))
