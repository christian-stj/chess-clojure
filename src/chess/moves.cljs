(ns chess.moves)

(defn move-piece [board from to]
  (let [from-file (first from)
        from-rank (second from)
        to-file (first to)
        to-rank (second to)
        piece (get-in board [from-file from-rank])]
    (-> board
        (assoc-in [to-file to-rank] piece)
        (update-in [from-file] dissoc from-rank))))

(defn- is-same-coordinate? [from to]
  (= from to))

(defn- is-legal-move-for-pawn? [board from to]
  (let [from-file (first from)
        from-rank (second from)
        to-file (first to)
        to-rank (second to)
        piece (get-in board [from-file from-rank])
        piece-at-destination (get-in board [to-file to-rank])
        is-on-base-rank? (or (and (= (:color piece) :white) (= from-rank :2))
                             (and (= (:color piece) :black) (= from-rank :7)))
        rank-diff (- (js/parseInt (name to-rank)) (js/parseInt (name from-rank)))
        rank-diff (if (= (:color piece) :white) rank-diff (- rank-diff))]
    (if (= from-file to-file) ; Moving straight
      (and (empty? piece-at-destination)
           (if is-on-base-rank?
             (>= 2 rank-diff 0)
             (>= 1 rank-diff 0)))
      (and (= 1 rank-diff) ; Capturing diagonally
           (not-empty piece-at-destination)
           (not= (:color piece-at-destination) (:color piece)))))) ; Moving diagonally must capture an opponent piece


(defn- is-legal-move-for-rook? [board from to]
  true)

(defn- is-legal-move-for-knight? [board from to]
  true)

(defn- is-legal-move-for-bishop? [board from to]
  true)

(defn- is-legal-move-for-queen? [board from to]
  true)

(defn- is-legal-move-for-king? [board from to]
  true)

(defn- is-legal-move-for-piece? [board from to]
  (let [piece (get-in board from)]
    (cond
      (= :pawn (:type piece)) (is-legal-move-for-pawn? board from to)
      (= :rook (:type piece)) (is-legal-move-for-rook? board from to)
      (= :knight (:type piece)) (is-legal-move-for-knight? board from to)
      (= :bishop (:type piece)) (is-legal-move-for-bishop? board from to)
      (= :queen (:type piece)) (is-legal-move-for-queen? board from to)
      (= :king (:type piece)) (is-legal-move-for-king? board from to))))

(defn- has-piece-of-same-color? [board from to]
  (let [from-piece (get-in board from)
        to-piece (get-in board to)]
    (and from-piece
         to-piece
         (= (:color from-piece) (:color to-piece)))))

(defn- places-king-in-check? [board from to]
  false)

(defn is-check? [board]
  false)

(defn- resolves-check? [board from to]
  true)

(defn is-legal-move? [board color-to-move from to]
  (let [piece (get-in board from)]
    (and piece
         (= (:color piece) color-to-move)
         (not (is-same-coordinate? from to))
         (not (has-piece-of-same-color? board from to))
         (not (places-king-in-check? board from to))
         (not (and (is-check? board)
                   (not (resolves-check? board from to))))
         (is-legal-move-for-piece? board from to))))
