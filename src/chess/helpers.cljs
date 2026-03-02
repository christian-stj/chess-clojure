(ns chess.helpers
  (:require
   [chess.definitions :refer [files ranks initial-board]]))


(defn square->indices [[file rank]]
  [(.indexOf files file) (.indexOf ranks rank)])

(defn indices->square [[file-index rank-index]]
  (when (and (<= 0 file-index 7) (<= 0 rank-index 7))
    [(nth files file-index) (nth ranks rank-index)]))

(defn same-square? [from to]
  (= from to))

(defn square-diff [from to]
  (mapv - (square->indices to) (square->indices from)))

(defn abs-square-diff [from to]
  (mapv (fn [n] (Math/abs n)) (square-diff from to)))

(defn within-one-square? [from to]
  (let [[fd rd] (abs-square-diff from to)]
    (and (<= fd 1) (<= rd 1))))

(defn- move-rook-for-castling [board king-from king-to]
  (let [[_ from-rank] king-from
        kingside? (> (first (square->indices king-to))
                     (first (square->indices king-from)))
        rook-from-file (if kingside? :h :a)
        rook-to-file   (if kingside? :f :d)
        rook-piece     (board [rook-from-file from-rank])]
    (-> board
        (assoc [rook-to-file from-rank] rook-piece)
        (dissoc [rook-from-file from-rank]))))

(defn- remove-en-passant-capture [board from to]
  (let [[to-file _] to
        [_ from-rank] from]
    (dissoc board [to-file from-rank])))

(defn- en-passant-capture? [board from to]
  (let [piece (board from)
        [from-file _] from
        [to-file _] to]
    (and (= (:type piece) :pawn)
         (not= from-file to-file)       ; diagonal move
         (nil? (board to)))))           ; no piece at destination

(defn move-piece [board from to]
  (let [piece (board from)
        castling? (and (= (:type piece) :king)
                       (= 2 (Math/abs (- (first (square->indices to))
                                         (first (square->indices from))))))
        en-passant? (en-passant-capture? board from to)]
    (cond-> (-> board
                (assoc to piece)
                (dissoc from))
      castling?   (move-rook-for-castling from to)
      en-passant? (remove-en-passant-capture from to))))

(def ^:private apply-moves
  (memoize
   (fn [moves]
     (reduce (fn [state move] (move-piece state (:from move) (:to move)))
             initial-board
             moves))))

(defn has-moved? [game-history square]
  (some (fn [move] (or (= (:from move) square) (= (:to move) square)))
        game-history))

(defn get-board-state [game-history]
  (apply-moves game-history))

(defn find-pieces
  "Returns a list of positions [file rank] whose piece satisfies `pred`, or an empty list if none."
  [board pred]
  (for [[square piece] board
        :when (pred piece)]
    square))

(defn find-piece
  "Returns the first square [file rank] whose piece satisfies `pred`, or nil."
  [board pred]
  (first (find-pieces board pred)))

(defn can-piece-reach?
  "Can piece at `piece-pos` legally move to `target`?"
  [move-list piece-pos movement-rule target]
  (let [piece ((get-board-state move-list) piece-pos)]
    (and piece
         (movement-rule move-list piece-pos target))))

(defn has-piece-of-same-color? [board from to]
  (let [from-piece (board from)
        to-piece   (board to)]
    (and from-piece
         to-piece
         (= (:color from-piece) (:color to-piece)))))

(defn get-color-to-move [game-history]
  (if (even? (count game-history)) :white :black))

(defn get-opponent-piece-positions [game-history]
  (let [board (get-board-state game-history)
        color-to-move (get-color-to-move game-history)
        opponent-color (if (= color-to-move :white) :black :white)
        opponent-piece? (fn [p] (and p (= (:color p) opponent-color)))]
    (find-pieces board opponent-piece?)))

(defn places-king-in-check? [move-list movement-rule from to]
  (let [simulated-move (conj move-list {:from from :to to})
        board (get-board-state move-list)
        simulated-board (get-board-state simulated-move)
        piece (board from)
        king-position (if (= (:type piece) :king)
                        to
                        (find-piece simulated-board (fn [p] (and (= (:type p) :king) (= (:color p) (:color piece))))))
        opponent-pieces (get-opponent-piece-positions move-list)]
    (some (fn [p] (can-piece-reach? simulated-move p movement-rule king-position))
          opponent-pieces)))

(defn- ray
  "Lazy seq of squares from `origin` (exclusive) stepping in `direction` until off-board."
  [origin [file-step rank-step]]
  (let [[file-index rank-index] (square->indices origin)]
    (->> (iterate (fn [[f r]] [(+ f file-step) (+ r rank-step)])
                  [(+ file-index file-step) (+ rank-index rank-step)])
         (take-while (fn [[f r]] (and (<= 0 f 7) (<= 0 r 7))))
         (map indices->square))))

(defn path-to
  "Squares between `from` and `to` (exclusive) along a ray in `direction`, or nil if unreachable."
  [from to direction]
  (let [squares-along-ray (ray from direction)]
    (when (some #{to} squares-along-ray)
      (take-while (fn [sq] (not= sq to)) squares-along-ray))))

(defn- path-clear? [board path]
  (every? (fn [sq] (nil? (board sq))) path))

(defn slides-to?
  "Can a piece slide from `from` to `to` along one of `directions` with a clear path?"
  [board from to directions]
  (some (fn [direction]
          (when-let [intermediate-squares (path-to from to direction)]
            (path-clear? board intermediate-squares)))
        directions))
