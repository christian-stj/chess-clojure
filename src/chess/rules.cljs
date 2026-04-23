(ns chess.rules
  (:require
   [chess.helpers :refer [abs-square-diff
                          can-piece-reach?
                          find-piece
                          find-pieces
                          get-board-state
                          get-color-to-move
                          has-moved?
                          has-piece-of-same-color?
                          path-to
                          reaches-last-rank?
                          same-square?
                          slides-to?
                          square-diff
                          places-king-in-check?
                          within-one-square?]]))

;; --- Directions ---

(def ^:private straight  [[0 1] [0 -1] [1 0] [-1 0]])
(def ^:private diagonal  [[1 1] [1 -1] [-1 1] [-1 -1]])

;; --- Move rules ---

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
  (let [[file-diff rank-diff] (abs-square-diff from to)]
    (or (and (= file-diff 2) (= rank-diff 1))
        (and (= file-diff 1) (= rank-diff 2)))))

(defn- bishop-move? [move-list from to]
  (slides-to? (get-board-state move-list) from to diagonal))

(defn- queen-move? [move-list from to]
  (let [board (get-board-state move-list)]
    (or (slides-to? board from to straight)
        (slides-to? board from to diagonal))))

;; Geometric movement rules: pure piece reach, no check-validation.
;; King is treated as attacking the eight adjacent squares only —
;; no castling, no recursion into check detection.
(def ^:private geometric-rules
  {:pawn   pawn-move?
   :rook   rook-move?
   :knight knight-move?
   :bishop bishop-move?
   :queen  queen-move?
   :king   (fn [_ from to] (within-one-square? from to))})

(defn- geometric-piece-move? [move-list from to]
  (when-let [rule (geometric-rules (:type ((get-board-state move-list) from)))]
    (rule move-list from to)))

(defn- square-attacked? [move-list square]
  (let [board (get-board-state move-list)
        color-to-move (get-color-to-move move-list)
        opponent-piece? (fn [p] (and p (not= (:color p) color-to-move)))
        opponent-positions (find-pieces board opponent-piece?)]
    (boolean (some (fn [pos] (can-piece-reach? move-list pos geometric-piece-move? square))
                   opponent-positions))))

(defn- king-move? [move-list from to]
  (let [[file-diff rank-diff] (square-diff from to)
        color-to-move (get-color-to-move move-list)
        on-base-position? (or (and (= color-to-move :white) (= from [:e :1]))
                              (and (= color-to-move :black) (= from [:e :8])))
        castling? (and on-base-position?
                       (= 0 rank-diff)
                       (= 2 file-diff))]
    (if castling?
      (let [queenside? (neg? file-diff)
            [_ from-rank] from
            rook-position (if queenside? [:a from-rank] [:h from-rank])
            path-to-target (path-to from to straight)]
        (and (not (some #(square-attacked? move-list %) path-to-target)) ; Cannot castle through check
             (not (has-moved? move-list from))           ; King must not have moved
             (not (has-moved? move-list rook-position))  ; Rook must not have moved
             (slides-to? (get-board-state move-list) from rook-position straight))) ; Path to rook must be clear
      (within-one-square? from to))))

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
        king-position (find-piece board (fn [p] (and (= (:type p) :king) (= (:color p) color-to-move))))]
    (square-attacked? move-list king-position)))

(def ^:private promotion-types #{:queen :rook :bishop :knight})

(defn pawn-reaching-last-rank?
  "Returns true if the piece at `from` is a pawn whose move to `to` reaches the promotion rank."
  [move-list from to]
  (let [board (get-board-state move-list)
        piece (board from)]
    (and piece
         (= (:type piece) :pawn)
         (= (:color piece) (get-color-to-move move-list))
         (reaches-last-rank? (:color piece) to))))

(defn- promotion-valid? [board from to promotion]
  (let [piece (board from)]
    (if (and (= (:type piece) :pawn)
             (reaches-last-rank? (:color piece) to))
      (contains? promotion-types promotion)
      (nil? promotion))))

(defn legal-move?
  ([move-list from to] (legal-move? move-list from to nil))
  ([move-list from to promotion]
   (let [board (get-board-state move-list)
         piece (board from)
         color-to-move (get-color-to-move move-list)
         current-player-to-move? (and piece (= (:color piece) color-to-move))]
     (and current-player-to-move?
          (not (same-square? from to))
          (not (has-piece-of-same-color? board from to))
          (promotion-valid? board from to promotion)
          (not (places-king-in-check? move-list geometric-piece-move? from to promotion))
          (legal-piece-move? move-list from to)))))
