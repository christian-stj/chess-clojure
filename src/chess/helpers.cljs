(ns chess.helpers
  (:require
   [chess.definitions :refer [files ranks initial-board]]))


(defn move-piece [board from to]
  (let [from-file (first from)
        from-rank (second from)
        to-file (first to)
        to-rank (second to)
        piece (get-in board [from-file from-rank])]
    (-> board
        (assoc-in [to-file to-rank] piece)
        (update-in [from-file] dissoc from-rank))))

(defn- apply-moves [moves]
  (reduce (fn [state move] (move-piece state (:from move) (:to move))) initial-board moves))

(defn get-board-state [game-history]
  ((memoize apply-moves) game-history))

(defn get-color-to-move [game-history]
  (if (even? (count game-history)) :white :black))


;; --- Board geometry ---

(defn square->indices [[file rank]]
  [(.indexOf files file) (.indexOf ranks rank)])

(defn indices->square [[file-index rank-index]]
  (when (and (<= 0 file-index 7) (<= 0 rank-index 7))
    [(nth files file-index) (nth ranks rank-index)]))

(defn- ray
  "Lazy seq of squares from `origin` (exclusive) stepping in `direction` until off-board."
  [origin [file-step rank-step]]
  (let [[file-index rank-index] (square->indices origin)]
    (->> (iterate (fn [[f r]] [(+ f file-step) (+ r rank-step)])
                  [(+ file-index file-step) (+ rank-index rank-step)])
         (take-while (fn [[f r]] (and (<= 0 f 7) (<= 0 r 7))))
         (map indices->square))))

(defn- path-to
  "Squares between `from` and `to` (exclusive) along a ray in `direction`, or nil if unreachable."
  [from to direction]
  (let [squares-along-ray (ray from direction)]
    (when (some #{to} squares-along-ray)
      (take-while #(not= % to) squares-along-ray))))

(defn- path-clear? [board path]
  (every? #(nil? (get-in board %)) path))

(defn slides-to?
  "Can a piece slide from `from` to `to` along one of `directions` with a clear path?"
  [board from to directions]
  (some (fn [direction]
          (when-let [intermediate-squares (path-to from to direction)]
            (path-clear? board intermediate-squares)))
        directions))
