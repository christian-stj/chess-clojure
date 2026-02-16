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
                          slides-to?
                          places-king-in-check?
                          square->indices]]))

(defn- is-same-coordinate? [from to]
  (= from to))

;; --- Directions ---

(def ^:private straight  [[0 1] [0 -1] [1 0] [-1 0]])
(def ^:private diagonal  [[1 1] [1 -1] [-1 1] [-1 -1]])

;; --- Move rules ---

(defn- is-legal-move-for-pawn? [move-list from to]
  (let [board (get-board-state move-list)
        from-file (first from)
        from-rank (second from)
        to-file (first to)
        to-rank (second to)
        piece (get-in board [from-file from-rank])
        piece-at-destination (get-in board [to-file to-rank])
        is-on-base-rank? (or (and (= (:color piece) :white) (= from-rank :2))
                             (and (= (:color piece) :black) (= from-rank :7)))
        [file-index-diff rank-index-diff] (map - (square->indices to) (square->indices from))
        rank-diff (if (= (:color piece) :white) rank-index-diff (- rank-index-diff))]
    (if (= from-file to-file) ; Moving straight
      (and (empty? piece-at-destination)
           (if is-on-base-rank?
             (>= 2 rank-diff 0)
             (>= 1 rank-diff 0)))
      (and (= 1 rank-diff) ; Capturing diagonally
           (= (Math/abs file-index-diff) 1)
           (not-empty piece-at-destination)
           (not= (:color piece-at-destination) (:color piece)))))) ; Moving diagonally must capture an opponent piece

(defn- is-legal-move-for-rook? [move-list from to]
  (slides-to? (get-board-state move-list) from to straight))

(defn- is-legal-move-for-knight? [_ from to]
    (let [[file-index-diff rank-index-diff] (map #(Math/abs %) (map - (square->indices to) (square->indices from)))]
        (or (and (= file-index-diff 2) (= rank-index-diff 1))
            (and (= file-index-diff 1) (= rank-index-diff 2)))))

(defn- is-legal-move-for-bishop? [move-list from to]
  (slides-to? (get-board-state move-list) from to diagonal))

(defn- is-legal-move-for-queen? [move-list from to]
  (let [board (get-board-state move-list)]
    (or (slides-to? board from to straight)
        (slides-to? board from to diagonal))))

(defn- is-legal-move-for-king? [move-list from to]
  (let [[file-index-diff rank-index-diff] (map - (square->indices to) (square->indices from))
        color-to-move (get-color-to-move move-list)
        is-on-base-position? (or (and (= color-to-move :white) (= from [:e :1]))
                                 (and (= color-to-move :black) (= from [:e :8])))
        is-opponent-piece? (fn [p] (and p (not= (:color p) color-to-move)))
        opponent-pieces (find-pieces (get-board-state move-list) is-opponent-piece?)
        opponent-king (first (filter (fn [p] (= (:type p) :king)) opponent-pieces))
        is-target-within-one-square-from-opponent-king? (and opponent-king
                                                             (let [[opp-file-index opp-rank-index] (square->indices opponent-king)
                                                                   [to-file-index to-rank-index] (square->indices to)]
                                                               (and (<= (Math/abs (- opp-file-index to-file-index)) 1)
                                                                    (<= (Math/abs (- opp-rank-index to-rank-index)) 1))))
        movement-rules-for-other-pieces {:pawn is-legal-move-for-pawn?
                                         :rook is-legal-move-for-rook?
                                         :knight is-legal-move-for-knight?
                                         :bishop is-legal-move-for-bishop?
                                         :queen is-legal-move-for-queen?
                                         :king (fn [_ _ _] false)} ; Opponent king's moves are not relevant for determining if our king is in check
        is-castling-move? (and is-on-base-position?
                               (= 0 rank-index-diff)
                               (= 2 (Math/abs file-index-diff)))]
    (and
     (not is-target-within-one-square-from-opponent-king?)
     (if is-castling-move?
       (let [is-queenside-castle? (neg? file-index-diff)
             rook-position (if is-queenside-castle?
                             [:a (second from)]
                             [:h (second from)])
             path-to-target (path-to from to straight)
             opponent-piece-positions (get-opponent-piece-positions move-list)]
         (and (not (some true?
                         (for [pos-on-path path-to-target
                               opponent-piece-pos opponent-piece-positions
                               :let [movement-rule (get movement-rules-for-other-pieces (:type (get-in (get-board-state move-list) opponent-piece-pos)))
                                     is-in-check? (can-piece-reach? move-list opponent-piece-pos movement-rule pos-on-path)]
                               :when is-in-check?]
                           (reduced true)))) ; Cannot castle through check
              (not (has-moved? move-list from)) ; King must not have moved
              (not (has-moved? move-list rook-position)) ; Rook must not have moved
              (slides-to? (get-board-state move-list) from rook-position straight))) ; Is the path to the rook clear?

       (and (<= (Math/abs file-index-diff) 1)
            (<= (Math/abs rank-index-diff) 1))))))

(defn- is-legal-move-for-piece? [move-list from to]
  (let [board (get-board-state move-list)
        piece (get-in board from)]
    (cond
      (= :pawn (:type piece)) (is-legal-move-for-pawn? move-list from to)
      (= :rook (:type piece)) (is-legal-move-for-rook? move-list from to)
      (= :knight (:type piece)) (is-legal-move-for-knight? move-list from to)
      (= :bishop (:type piece)) (is-legal-move-for-bishop? move-list from to)
      (= :queen (:type piece)) (is-legal-move-for-queen? move-list from to)
      (= :king (:type piece)) (is-legal-move-for-king? move-list from to))))

;; --- Board queries ---

(defn is-check? [move-list]
  (let [board (get-board-state move-list)
        color-to-move (get-color-to-move move-list)
        king-position (find-piece board #(and (= (:type %) :king) (= (:color %) color-to-move)))
        opponent-piece-positions (get-opponent-piece-positions move-list)]
    (some true?
          (map
           (fn [p] (can-piece-reach? move-list p is-legal-move-for-piece? king-position))
           opponent-piece-positions))))

(defn is-legal-move? [move-list from to]
  (let [board (get-board-state move-list)
        piece (get-in board from)
        color-to-move (get-color-to-move move-list)
        is-color-to-move? (and piece (= (:color piece) color-to-move))]
    (and is-color-to-move?
         (not (is-same-coordinate? from to))
         (not (has-piece-of-same-color? board from to))
         (not (places-king-in-check? move-list is-legal-move-for-piece? from to)) ; Resolving check is handled by not placing the king in check again
         (is-legal-move-for-piece? move-list from to))))
