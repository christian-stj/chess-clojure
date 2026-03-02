(ns chess.rules
  (:require
   [chess.helpers :refer [abs-square-diff
                          can-piece-reach?
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
                          square-diff
                          places-king-in-check?
                          within-one-square?]]))

;; --- Directions ---

(def ^:private straight  [[0 1] [0 -1] [1 0] [-1 0]])
(def ^:private diagonal  [[1 1] [1 -1] [-1 1] [-1 -1]])

;; --- Move rules ---

(declare move-rules)

(defn- last-move-was-double-pawn-push? [board move-list to-file]
  (when-let [{last-from :from [last-to-file :as last-to] :to} (peek move-list)]
    (let [last-piece (board last-to)
          [_ rank-diff] (abs-square-diff last-from last-to)]
      (and last-piece
           (= (:type last-piece) :pawn)
           (= rank-diff 2)
           (= last-to-file to-file)))))

(defn- en-passant? [move-list board from to]
  (let [color (:color (board from))
        [_ from-rank] from
        [to-file _] to
        [file-diff rank-diff] (abs-square-diff from to)
        on-en-passant-rank? (or (and (= color :white) (= from-rank :5))
                                (and (= color :black) (= from-rank :4)))]
    (and on-en-passant-rank?
         (= rank-diff 1)
         (= file-diff 1)
         (nil? (board to))
         (last-move-was-double-pawn-push? board move-list to-file))))

(defn- pawn-move? [move-list from to]
  (let [board (get-board-state move-list)
        [from-file from-rank] from
        [to-file _] to
        piece (board from)
        color (:color piece)
        [file-diff rank-diff] (square-diff from to)
        forward? (if (= color :white) (pos? rank-diff) (neg? rank-diff))
        rank-diff (Math/abs rank-diff)
        file-diff (Math/abs file-diff)
        piece-at-destination (board to)
        on-base-rank? (or (and (= color :white) (= from-rank :2))
                          (and (= color :black) (= from-rank :7)))]
    (and forward?
         (if (= from-file to-file) ; Moving straight
           (and (nil? piece-at-destination)
                (if on-base-rank?
                  (<= rank-diff 2)
                  (<= rank-diff 1)))
           (and (= rank-diff 1) ; Capturing diagonally
                (= file-diff 1)
                (or (and (some? piece-at-destination)
                         (not= (:color piece-at-destination) color))
                    (en-passant? move-list board from to)))))))

(defn- rook-move? [move-list from to]
  (slides-to? (get-board-state move-list) from to straight))

(defn- knight-move? [_ from to]
    (let [[file-index-diff rank-index-diff] (abs-square-diff from to)]
        (or (and (= file-index-diff 2) (= rank-index-diff 1))
            (and (= file-index-diff 1) (= rank-index-diff 2)))))

(defn- bishop-move? [move-list from to]
  (slides-to? (get-board-state move-list) from to diagonal))

(defn- queen-move? [move-list from to]
  (let [board (get-board-state move-list)]
    (or (slides-to? board from to straight)
        (slides-to? board from to diagonal))))

(defn- king-move? [move-list from to]
  (let [[file-index-diff rank-index-diff] (square-diff from to)
        color-to-move (get-color-to-move move-list)
        on-base-position? (or (and (= color-to-move :white) (= from [:e :1]))
                           (and (= color-to-move :black) (= from [:e :8])))
        board (get-board-state move-list)
        opponent-piece? (fn [p] (and p (not= (:color p) color-to-move)))
        opponent-pieces (find-pieces board opponent-piece?)
        opponent-king (first (filter (fn [sq] (= (:type (board sq)) :king)) opponent-pieces))
        target-within-one-square-of-opponent-king? (and opponent-king (within-one-square? opponent-king to))
        movement-rules-for-other-pieces (assoc move-rules :king (fn [_ _ _] false)) ; Opponent king's moves are not relevant for determining if our king is in check
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

       (within-one-square? from to)))))

(def ^:private move-rules
  {:pawn   pawn-move?
   :rook   rook-move?
   :knight knight-move?
   :bishop bishop-move?
   :queen  queen-move?
   :king   king-move?})

(defn- legal-piece-move? [move-list from to]
  (when-let [movement-rule (move-rules (:type ((get-board-state move-list) from)))]
    (movement-rule move-list from to)))

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
