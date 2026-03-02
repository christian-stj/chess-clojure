(ns chess.helpers-test
  (:require
   [cljs.test :refer [deftest testing is]]
   [chess.definitions :refer [initial-board]]
   [chess.helpers :refer [square->indices
                          indices->square
                          move-piece
                          get-board-state
                          get-color-to-move
                          find-piece
                          find-pieces
                          has-piece-of-same-color?
                          has-moved?]]))

;; --- square->indices / indices->square ---

(deftest square->indices-test
  (is (= [0 0] (square->indices [:a :1])))
  (is (= [4 3] (square->indices [:e :4])))
  (is (= [7 7] (square->indices [:h :8]))))

(deftest indices->square-test
  (is (= [:a :1] (indices->square [0 0])))
  (is (= [:e :4] (indices->square [4 3])))
  (is (= [:h :8] (indices->square [7 7])))
  (is (nil? (indices->square [-1 0])))
  (is (nil? (indices->square [0 8]))))

;; --- move-piece ---

(deftest move-piece-test
  (testing "basic move"
    (let [board (move-piece initial-board [:e :2] [:e :4])]
      (is (= {:type :pawn :color :white} (board [:e :4])))
      (is (nil? (board [:e :2])))))

  (testing "capture replaces piece"
    (let [board (-> initial-board
                    (move-piece [:e :2] [:e :4])
                    (move-piece [:d :7] [:d :5])
                    (move-piece [:e :4] [:d :5]))]
      (is (= {:type :pawn :color :white} (board [:d :5])))
      (is (nil? (board [:e :4])))))

  (testing "kingside castling moves rook"
    (let [board (-> initial-board
                    (dissoc [:f :1] [:g :1])  ; clear path
                    (move-piece [:e :1] [:g :1]))]
      (is (= {:type :king :color :white} (board [:g :1])))
      (is (= {:type :rook :color :white} (board [:f :1])))
      (is (nil? (board [:e :1])))
      (is (nil? (board [:h :1])))))

  (testing "queenside castling moves rook"
    (let [board (-> initial-board
                    (dissoc [:b :1] [:c :1] [:d :1])  ; clear path
                    (move-piece [:e :1] [:c :1]))]
      (is (= {:type :king :color :white} (board [:c :1])))
      (is (= {:type :rook :color :white} (board [:d :1])))
      (is (nil? (board [:e :1])))
      (is (nil? (board [:a :1]))))))

;; --- get-board-state ---

(deftest get-board-state-test
  (testing "empty history returns initial board"
    (is (= initial-board (get-board-state []))))

  (testing "replays moves"
    (let [history [{:from [:e :2] :to [:e :4]}
                   {:from [:e :7] :to [:e :5]}]
          board (get-board-state history)]
      (is (= {:type :pawn :color :white} (board [:e :4])))
      (is (= {:type :pawn :color :black} (board [:e :5])))
      (is (nil? (board [:e :2])))
      (is (nil? (board [:e :7]))))))

;; --- get-color-to-move ---

(deftest get-color-to-move-test
  (is (= :white (get-color-to-move [])))
  (is (= :black (get-color-to-move [{:from [:e :2] :to [:e :4]}])))
  (is (= :white (get-color-to-move [{:from [:e :2] :to [:e :4]}
                                     {:from [:e :7] :to [:e :5]}]))))

;; --- find-piece / find-pieces ---

(deftest find-piece-test
  (let [king-sq (find-piece initial-board
                            (fn [p] (and (= (:type p) :king) (= (:color p) :white))))]
    (is (= [:e :1] king-sq))))

(deftest find-pieces-test
  (let [white-pawns (find-pieces initial-board
                                 (fn [p] (and (= (:type p) :pawn) (= (:color p) :white))))]
    (is (= 8 (count white-pawns)))
    (is (every? (fn [[_ rank]] (= :2 rank)) white-pawns))))

;; --- has-piece-of-same-color? ---

(deftest has-piece-of-same-color?-test
  (testing "two white pieces"
    (is (true? (has-piece-of-same-color? initial-board [:a :1] [:b :1]))))
  (testing "white and black"
    (is (not (has-piece-of-same-color? initial-board [:a :1] [:a :8]))))
  (testing "empty destination"
    (is (not (has-piece-of-same-color? initial-board [:a :1] [:a :4])))))

;; --- has-moved? ---

(deftest has-moved?-test
  (let [history [{:from [:e :2] :to [:e :4]}]]
    (is (has-moved? history [:e :2]))
    (is (has-moved? history [:e :4]))
    (is (not (has-moved? history [:d :2])))))
