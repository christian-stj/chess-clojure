(ns chess.rules
  (:require [chess.helpers :refer [get-board-state
                                   get-color-to-move
                                   slides-to?
                                   square->indices]]
            [chess.definitions :refer [files ranks]]))

(defn- is-same-coordinate? [from to]
  (= from to))

;; --- Directions ---

(def ^:private straight  [[0 1] [0 -1] [1 0] [-1 0]])
(def ^:private diagonal  [[1 1] [1 -1] [-1 1] [-1 -1]])

;; --- Piece types ---

(defn- is-legal-move-for-pawn? [board from to]
  (let [from-file (first from)
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

(defn- is-legal-move-for-rook? [board from to]
  (slides-to? board from to straight))

(defn- is-legal-move-for-knight? [from to]
    (let [[file-index-diff rank-index-diff] (map #(Math/abs %) (map - (square->indices to) (square->indices from)))]
        (or (and (= file-index-diff 2) (= rank-index-diff 1))
            (and (= file-index-diff 1) (= rank-index-diff 2)))))

(defn- is-legal-move-for-bishop? [board from to]
  (slides-to? board from to diagonal))

(defn- is-legal-move-for-queen? [board from to]
  (or (slides-to? board from to straight)
      (slides-to? board from to diagonal)))

(defn- is-legal-move-for-king? [from to]
  (let [[file-index-diff rank-index-diff] (map - (square->indices to) (square->indices from))]
    (and (<= (Math/abs file-index-diff) 1)
         (<= (Math/abs rank-index-diff) 1))))

(defn- is-legal-move-for-piece? [board from to]
  (let [piece (get-in board from)]
    (cond
      (= :pawn (:type piece)) (is-legal-move-for-pawn? board from to)
      (= :rook (:type piece)) (is-legal-move-for-rook? board from to)
      (= :knight (:type piece)) (is-legal-move-for-knight? from to)
      (= :bishop (:type piece)) (is-legal-move-for-bishop? board from to)
      (= :queen (:type piece)) (is-legal-move-for-queen? board from to)
      (= :king (:type piece)) (is-legal-move-for-king? from to))))

;; --- Board queries ---

(defn- find-piece
  "Returns the first square [file rank] whose piece satisfies `pred`, or nil."
  [board pred]
  (some (fn [[file rank]]
          (let [piece (get-in board [file rank])]
            (when (and piece (pred piece))
              [file rank])))
        (for [file files rank ranks] [file rank])))

(defn- any-piece-can-reach?
  "Is there any piece on the board satisfying `piece-pred` that can legally move to `target`?"
  [board piece-pred target]
  (some (fn [[file rank]]
          (let [piece (get-in board [file rank])]
            (when (and piece
                       (piece-pred piece)
                       (is-legal-move-for-piece? board [file rank] target))
              true)))
        (for [file files rank ranks] [file rank])))

(defn- has-piece-of-same-color? [board from to]
  (let [from-piece (get-in board from)
        to-piece (get-in board to)]
    (and from-piece
         to-piece
         (= (:color from-piece) (:color to-piece)))))

(defn- places-king-in-check? [board from to]
  false)

(defn is-check? [move-list]
  (let [board (get-board-state move-list)
        color-to-move (get-color-to-move move-list)
        king-position (find-piece board #(and (= (:type %) :king) (= (:color %) color-to-move)))]
    (any-piece-can-reach? board #(not= (:color %) color-to-move) king-position)))

(defn- resolves-check? [board from to]
  true)

(defn is-legal-move? [move-list from to]
  (let [board (get-board-state move-list)
        piece (get-in board from)
        color-to-move (get-color-to-move move-list)
        is-color-to-move? (and piece (= (:color piece) color-to-move))]
    (and is-color-to-move?
         (not (is-same-coordinate? from to))
         (not (has-piece-of-same-color? board from to))
         (not (places-king-in-check? board from to))
         (or (not (is-check? move-list))
             (resolves-check? board from to))
         (is-legal-move-for-piece? board from to))))
