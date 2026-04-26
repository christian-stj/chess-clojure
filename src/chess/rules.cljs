(ns chess.rules
  (:require
   [chess.helpers :refer [abs-square-diff
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
                          within-one-square?]]))

;; --- Directions ---

(def ^:private straight  [[0 1] [0 -1] [1 0] [-1 0]])
(def ^:private diagonal  [[1 1] [1 -1] [-1 1] [-1 -1]])

;; --- Move rules ---

(defn- last-move-was-double-pawn-push? [board history to-file]
  (when-let [{last-from :from [last-to-file :as last-to] :to} (peek history)]
    (let [last-piece (board last-to)
          [_ rank-diff] (abs-square-diff last-from last-to)]
      (and last-piece
           (= (:type last-piece) :pawn)
           (= rank-diff 2)
           (= last-to-file to-file)))))

(defn- en-passant? [history board from to]
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
         (last-move-was-double-pawn-push? board history to-file))))

(defn- pawn-push? [board from to color [file-diff rank-diff]]
  (let [[_ from-rank] from
        on-base-rank? (or (and (= color :white) (= from-rank :2))
                          (and (= color :black) (= from-rank :7)))]
    (and (zero? file-diff)
         (nil? (board to))
         (if on-base-rank?
           (<= rank-diff 2)
           (= rank-diff 1)))))

(defn- pawn-capture? [history board from to color [file-diff rank-diff]]
  (and (= file-diff 1)
       (= rank-diff 1)
       (or (some-> (board to) :color (not= color))
           (en-passant? history board from to))))

(defn- pawn-move? [history from to]
  (let [board (get-board-state history)
        piece (board from)
        color (:color piece)
        [_ signed-rank-diff] (square-diff from to)
        moving-forward? (if (= color :white) (pos? signed-rank-diff) (neg? signed-rank-diff))
        diffs (abs-square-diff from to)]
    (and moving-forward?
         (or (pawn-push? board from to color diffs)
             (pawn-capture? history board from to color diffs)))))

(defn- rook-move? [history from to]
  (slides-to? (get-board-state history) from to straight))

(defn- knight-move? [_ from to]
  (let [[file-diff rank-diff] (abs-square-diff from to)]
    (or (and (= file-diff 2) (= rank-diff 1))
        (and (= file-diff 1) (= rank-diff 2)))))

(defn- bishop-move? [history from to]
  (slides-to? (get-board-state history) from to diagonal))

(defn- queen-move? [history from to]
  (let [board (get-board-state history)]
    (or (slides-to? board from to straight)
        (slides-to? board from to diagonal))))

;; King is treated as attacking the eight adjacent squares only —
;; no castling, no recursion into check detection.
(def ^:private geometric-rules
  {:pawn   pawn-move?
   :rook   rook-move?
   :knight knight-move?
   :bishop bishop-move?
   :queen  queen-move?
   :king   (fn [_ from to] (within-one-square? from to))})

(defn- geometric-piece-move? [history from to]
  (when-let [rule (geometric-rules (:type ((get-board-state history) from)))]
    (rule history from to)))

(defn- can-piece-reach?
  "Can piece at `piece-pos` reach `target` according to `movement-rule`?"
  [history piece-pos movement-rule target]
  (let [piece ((get-board-state history) piece-pos)]
    (and piece
         (movement-rule history piece-pos target))))

(defn- square-attacked-by? [history attacker-color square]
  (let [board (get-board-state history)
        attacker? (fn [p] (and p (= (:color p) attacker-color)))
        attacker-positions (find-pieces board attacker?)]
    (some (fn [pos] (can-piece-reach? history pos geometric-piece-move? square))
          attacker-positions)))

(defn- places-king-in-check?
  ([history from to]
   (places-king-in-check? history from to nil))
  ([history from to promotion]
   (let [simulated-history (conj history (if promotion
                                           {:from from :to to :promotion promotion}
                                           {:from from :to to}))
         piece ((get-board-state history) from)
         color (:color piece)
         opponent-color (if (= color :white) :black :white)
         simulated-board (get-board-state simulated-history)
         king-position (if (= (:type piece) :king)
                         to
                         (find-piece simulated-board (fn [p] (and (= (:type p) :king) (= (:color p) color)))))]
     (square-attacked-by? simulated-history opponent-color king-position))))

(defn- king-move? [history from to]
  (let [[file-diff rank-diff] (square-diff from to)
        color-to-move (get-color-to-move history)
        opponent-color (if (= color-to-move :white) :black :white)
        on-base-position? (or (and (= color-to-move :white) (= from [:e :1]))
                              (and (= color-to-move :black) (= from [:e :8])))
        castling? (and on-base-position?
                       (= 0 rank-diff)
                       (= 2 (Math/abs file-diff)))]
    (if-not castling?
      (within-one-square? from to)
      (let [queenside? (neg? file-diff)
            [_ from-rank] from
            rook-position (if queenside? [:a from-rank] [:h from-rank])
            path-to-target (path-to from to straight)]
        (and (not (some #(square-attacked-by? history opponent-color %) path-to-target)) ; Cannot castle through check
             (not (has-moved? history from))           ; King must not have moved
             (not (has-moved? history rook-position))  ; Rook must not have moved
             (slides-to? (get-board-state history) from rook-position straight))) ; Path to rook must be clear
      )))

(def ^:private move-rules
  {:pawn   pawn-move?
   :rook   rook-move?
   :knight knight-move?
   :bishop bishop-move?
   :queen  queen-move?
   :king   king-move?})

(defn- legal-piece-move? [history from to]
  (when-let [movement-rule (move-rules (:type ((get-board-state history) from)))]
    (movement-rule history from to)))

;; --- Board queries ---

(defn in-check? [history]
  (let [board (get-board-state history)
        color-to-move (get-color-to-move history)
        opponent-color (if (= color-to-move :white) :black :white)
        king-position (find-piece board (fn [p] (and (= (:type p) :king) (= (:color p) color-to-move))))]
    (square-attacked-by? history opponent-color king-position)))

(def ^:private promotion-types #{:queen :rook :bishop :knight})

(defn pawn-reaching-last-rank?
  "Returns true if the piece at `from` is a pawn whose move to `to` reaches the promotion rank."
  [history from to]
  (let [board (get-board-state history)
        piece (board from)]
    (and piece
         (= (:type piece) :pawn)
         (= (:color piece) (get-color-to-move history))
         (reaches-last-rank? (:color piece) to))))

(defn- promotion-valid? [board from to promotion]
  (let [piece (board from)]
    (if (and (= (:type piece) :pawn)
             (reaches-last-rank? (:color piece) to))
      (contains? promotion-types promotion)
      (nil? promotion))))

(defn legal-move?
  ([history from to] (legal-move? history from to nil))
  ([history from to promotion]
   (let [board (get-board-state history)
         piece (board from)
         color-to-move (get-color-to-move history)
         current-player-to-move? (and piece (= (:color piece) color-to-move))]
     (and current-player-to-move?
          (not (same-square? from to))
          (not (has-piece-of-same-color? board from to))
          (promotion-valid? board from to promotion)
          (not (places-king-in-check? history from to promotion))
          (legal-piece-move? history from to)))))
