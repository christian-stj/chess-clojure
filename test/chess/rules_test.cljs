(ns chess.rules-test
  (:require
   [cljs.test :refer [deftest testing is]]
   [chess.definitions :refer [initial-board]]
   [chess.rules :refer [legal-move? in-check?]]))

;; --- Pawn moves ---

(deftest pawn-moves-test
  (testing "white pawn can advance one square"
    (is (legal-move? [] [:e :2] [:e :3])))

  (testing "white pawn can advance two squares from base rank"
    (is (legal-move? [] [:e :2] [:e :4])))

  (testing "white pawn cannot advance three squares"
    (is (not (legal-move? [] [:e :2] [:e :5]))))

  (testing "white pawn cannot move backward"
    (let [history [{:from [:e :2] :to [:e :4]}
                   {:from [:a :7] :to [:a :6]}]]
      (is (not (legal-move? history [:e :4] [:e :3])))))

  (testing "pawn cannot advance into occupied square"
    (let [history [{:from [:e :2] :to [:e :4]}
                   {:from [:e :7] :to [:e :5]}]]
      (is (not (legal-move? history [:e :4] [:e :5])))))

  (testing "pawn captures diagonally"
    (let [history [{:from [:e :2] :to [:e :4]}
                   {:from [:d :7] :to [:d :5]}]]
      (is (legal-move? history [:e :4] [:d :5]))))

  (testing "pawn cannot move diagonally without capture"
    (is (not (legal-move? [] [:e :2] [:d :3])))))

;; --- Knight moves ---

(deftest knight-moves-test
  (testing "knight can jump in L-shape"
    (is (legal-move? [] [:b :1] [:c :3]))
    (is (legal-move? [] [:b :1] [:a :3])))

  (testing "knight cannot move like other pieces"
    (is (not (legal-move? [] [:b :1] [:b :3])))
    (is (not (legal-move? [] [:b :1] [:d :2])))))

;; --- Bishop moves ---

(deftest bishop-moves-test
  (testing "bishop can move diagonally when path is clear"
    (let [history [{:from [:d :2] :to [:d :4]}
                   {:from [:a :7] :to [:a :6]}]]
      (is (legal-move? history [:c :1] [:f :4]))))

  (testing "bishop cannot move through pieces"
    (is (not (legal-move? [] [:c :1] [:e :3])))))

;; --- Rook moves ---

(deftest rook-moves-test
  (testing "rook can move along file when clear"
    (let [history [{:from [:a :2] :to [:a :4]}
                   {:from [:a :7] :to [:a :5]}]]
      ;; now a-file is partially clear; rook can reach a2's old square
      (is (legal-move? history [:a :1] [:a :3]))))

  (testing "rook cannot move diagonally"
    (let [history [{:from [:a :2] :to [:a :4]}
                   {:from [:a :7] :to [:a :6]}]]
      (is (not (legal-move? history [:a :1] [:b :2]))))))

;; --- Queen moves ---

(deftest queen-moves-test
  (testing "queen moves straight when path clear"
    (let [history [{:from [:d :2] :to [:d :4]}
                   {:from [:a :7] :to [:a :6]}]]
      (is (legal-move? history [:d :1] [:d :3]))))

  (testing "queen moves diagonally when path clear"
    (let [history [{:from [:e :2] :to [:e :4]}
                   {:from [:a :7] :to [:a :6]}]]
      (is (legal-move? history [:d :1] [:h :5])))))

;; --- King moves ---

(deftest king-moves-test
  (testing "king can move one square"
    (let [history [{:from [:e :2] :to [:e :4]}
                   {:from [:a :7] :to [:a :6]}]]
      (is (legal-move? history [:e :1] [:e :2]))))

  (testing "king cannot move two squares (non-castling)"
    (let [history [{:from [:e :2] :to [:e :4]}
                   {:from [:a :7] :to [:a :6]}]]
      (is (not (legal-move? history [:e :1] [:e :3]))))))

;; --- General legality ---

(deftest general-legality-test
  (testing "cannot move opponent's piece"
    (is (not (legal-move? [] [:e :7] [:e :5]))))

  (testing "cannot capture own piece"
    (is (not (legal-move? [] [:a :1] [:a :2]))))

  (testing "cannot move to same square"
    (is (not (legal-move? [] [:e :2] [:e :2])))))

;; --- Check ---

(deftest check-test
  (testing "not in check at start"
    (is (not (in-check? []))))

  (testing "scholar's mate check"
    ;; Set up a position where black is in check
    (let [history [{:from [:e :2] :to [:e :4]}   ; white e4
                   {:from [:e :7] :to [:e :5]}   ; black e5
                   {:from [:d :1] :to [:h :5]}   ; white Qh5
                   {:from [:b :8] :to [:c :6]}   ; black Nc6
                   {:from [:f :1] :to [:c :4]}   ; white Bc4
                   {:from [:g :8] :to [:f :6]}]] ; black Nf6
      ;; After Nf6, it's white's turn — white is not in check
      (is (not (in-check? history)))))

  (testing "cannot move into check"
    ;; King cannot move to a square attacked by opponent
    (let [history [{:from [:e :2] :to [:e :4]}
                   {:from [:d :7] :to [:d :5]}
                   {:from [:e :1] :to [:e :2]}
                   {:from [:d :5] :to [:e :4]}]]
      ;; white king on e2, black pawn on e4 — king can't go to d3 or f3 (pawn attacks)
      (is (not (legal-move? history [:e :2] [:d :3]))))))

;; --- Castling ---

(deftest castling-test
  (testing "kingside castling when path is clear and nothing has moved"
    ;; Clear f1, g1 by playing moves that get them out of the way
    (let [history [{:from [:g :1] :to [:f :3]}   ; white Nf3
                   {:from [:a :7] :to [:a :6]}   ; black a6
                   {:from [:e :2] :to [:e :4]}   ; white e4
                   {:from [:b :7] :to [:b :6]}   ; black b6
                   {:from [:f :1] :to [:c :4]}   ; white Bc4
                   {:from [:c :7] :to [:c :6]}]] ; black c6
      ;; Now f1 and g1 are clear, king and rook haven't moved
      (is (legal-move? history [:e :1] [:g :1]))))

  (testing "cannot castle after king has moved"
    (let [history [{:from [:g :1] :to [:f :3]}
                   {:from [:a :7] :to [:a :6]}
                   {:from [:e :2] :to [:e :4]}
                   {:from [:b :7] :to [:b :6]}
                   {:from [:f :1] :to [:c :4]}
                   {:from [:c :7] :to [:c :6]}
                   {:from [:e :1] :to [:e :2]}   ; king moves
                   {:from [:d :7] :to [:d :6]}
                   {:from [:e :2] :to [:e :1]}   ; king moves back
                   {:from [:h :7] :to [:h :6]}]]
      (is (not (legal-move? history [:e :1] [:g :1]))))))
