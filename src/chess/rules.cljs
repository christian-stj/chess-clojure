(ns chess.rules
  (:require
   [chess.helpers :refer [can-piece-reach?
                          find-piece
                          find-pieces
                          get-board-state
                          get-color-to-move
                          get-opponent-piece-positions
                          has-moved?
                          has-piece-of-same-color?
                          path-to
                          same-square?
                          slides-to?
                          places-king-in-check?
                          square->indices]]))

;; --- Directions ---

(def ^:private straight  [[0 1] [0 -1] [1 0] [-1 0]])
(def ^:private diagonal  [[1 1] [1 -1] [-1 1] [-1 -1]])

;; --- Move rules ---

(defn- pawn-move? [move-list from to]
  (let [board (get-board-state move-list)
        [from-file from-rank] from
        [to-file _] to
        piece (board from)
        piece-at-destination (board to)
        on-base-rank? (or (and (= (:color piece) :white) (= from-rank :2))
                       (and (= (:color piece) :black) (= from-rank :7)))
        [file-index-diff rank-index-diff] (map - (square->indices to) (square->indices from))
        rank-diff (if (= (:color piece) :white) rank-index-diff (- rank-index-diff))]
    (if (= from-file to-file) ; Moving straight
      (and (nil? piece-at-destination)
           (if on-base-rank?
             (>= 2 rank-diff 0)
             (>= 1 rank-diff 0)))
      (and (= 1 rank-diff) ; Capturing diagonally
           (= (Math/abs file-index-diff) 1)
           (some? piece-at-destination)
           (not= (:color piece-at-destination) (:color piece)))))) ; Moving diagonally must capture an opponent piece

(defn- rook-move? [move-list from to]
  (slides-to? (get-board-state move-list) from to straight))

(defn- knight-move? [_ from to]
    (let [[file-index-diff rank-index-diff] (map (fn [n] (Math/abs n)) (map - (square->indices to) (square->indices from)))]
        (or (and (= file-index-diff 2) (= rank-index-diff 1))
            (and (= file-index-diff 1) (= rank-index-diff 2)))))

(defn- bishop-move? [move-list from to]
  (slides-to? (get-board-state move-list) from to diagonal))

(defn- queen-move? [move-list from to]
  (let [board (get-board-state move-list)]
    (or (slides-to? board from to straight)
        (slides-to? board from to diagonal))))

(defn- king-move? [move-list from to]
  (let [[file-index-diff rank-index-diff] (map - (square->indices to) (square->indices from))
        color-to-move (get-color-to-move move-list)
        on-base-position? (or (and (= color-to-move :white) (= from [:e :1]))
                           (and (= color-to-move :black) (= from [:e :8])))
        board (get-board-state move-list)
        opponent-piece? (fn [p] (and p (not= (:color p) color-to-move)))
        opponent-pieces (find-pieces board opponent-piece?)
        opponent-king (first (filter (fn [sq] (= (:type (board sq)) :king)) opponent-pieces))
        target-within-one-square-of-opponent-king? (and opponent-king
                                 (let [[opp-file-index opp-rank-index] (square->indices opponent-king)
                                       [to-file-index to-rank-index] (square->indices to)]
                                   (and (<= (Math/abs (- opp-file-index to-file-index)) 1)
                                        (<= (Math/abs (- opp-rank-index to-rank-index)) 1))))
        movement-rules-for-other-pieces {:pawn pawn-move?
                                         :rook rook-move?
                                         :knight knight-move?
                                         :bishop bishop-move?
                                         :queen queen-move?
                                         :king (fn [_ _ _] false)} ; Opponent king's moves are not relevant for determining if our king is in check
        castling? (and on-base-position?
                       (= 0 rank-index-diff)
                       (= 2 (Math/abs file-index-diff)))]
    (and
     (not target-within-one-square-of-opponent-king?)
     (if castling?
       (let [queenside? (neg? file-index-diff)
             [_ from-rank] from
             rook-position (if queenside?
                             [:a from-rank]
                             [:h from-rank])
             path-to-target (path-to from to straight)
             opponent-piece-positions (get-opponent-piece-positions move-list)]
         (and (not (some (fn [pos-on-path]
                          (some (fn [opponent-piece-pos]
                                  (let [movement-rule (get movement-rules-for-other-pieces
                                                           (:type ((get-board-state move-list) opponent-piece-pos)))]
                                    (can-piece-reach? move-list opponent-piece-pos movement-rule pos-on-path)))
                                opponent-piece-positions))
                        path-to-target)) ; Cannot castle through check
              (not (has-moved? move-list from)) ; King must not have moved
              (not (has-moved? move-list rook-position)) ; Rook must not have moved
              (slides-to? (get-board-state move-list) from rook-position straight))) ; Is the path to the rook clear?

       (and (<= (Math/abs file-index-diff) 1)
            (<= (Math/abs rank-index-diff) 1))))))

(defn- legal-piece-move? [move-list from to]
  (let [piece ((get-board-state move-list) from)]
    (cond
      (= :pawn (:type piece)) (pawn-move? move-list from to)
      (= :rook (:type piece)) (rook-move? move-list from to)
      (= :knight (:type piece)) (knight-move? move-list from to)
      (= :bishop (:type piece)) (bishop-move? move-list from to)
      (= :queen (:type piece)) (queen-move? move-list from to)
      (= :king (:type piece)) (king-move? move-list from to))))

;; --- Board queries ---

(defn in-check? [move-list]
  (let [board (get-board-state move-list)
        color-to-move (get-color-to-move move-list)
        king-position (find-piece board (fn [p] (and (= (:type p) :king) (= (:color p) color-to-move))))
        opponent-piece-positions (get-opponent-piece-positions move-list)]
    (some (fn [p] (can-piece-reach? move-list p legal-piece-move? king-position))
          opponent-piece-positions)))

(defn legal-move? [move-list from to]
  (let [board (get-board-state move-list)
        piece (board from)
        color-to-move (get-color-to-move move-list)
        current-player-to-move? (and piece (= (:color piece) color-to-move))]
    (and current-player-to-move?
         (not (same-square? from to))
         (not (has-piece-of-same-color? board from to))
         (not (places-king-in-check? move-list legal-piece-move? from to)) ; Resolving check is handled by not placing the king in check again
         (legal-piece-move? move-list from to))))
