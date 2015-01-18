
;; Analyze a Tic-Tac-Toe Board

;; A tic-tac-toe board is represented by a two dimensional vector. X is represented by :x, O is represented by :o, 
;; and empty is represented by :e. A player wins by placing three Xs or three Os in a horizontal, vertical, or diagonal row. 
;; Write a function which analyzes a tic-tac-toe board and returns :x if X has won, :o if O has won, and nil if neither player has won.

(defn attt [b]
  (let [win-pos [[[0 0] [0 1] [0 2]]  ; 1st row
                 [[1 0] [1 1] [1 2]]  ; 2nd row
                 [[2 0] [2 1] [2 2]]  ; 3rd row
                 [[0 0] [1 0] [2 0]]  ; 1st column
                 [[0 1] [1 1] [2 1]]  ; 2nd column
                 [[0 2] [1 2] [2 2]]  ; 3rd column
                 [[0 0] [1 1] [2 2]]  ; left to right diagonale
                 [[0 2] [1 1] [2 0]]] ; right to left diagonale
        get-value-at (fn [b p]
                       ((b (p 0)) (p 1)))
        get-values-at (fn [b ps]
                        (loop [ps1 ps, result []]
                          (if (empty? ps1)
                            result
                            (recur (rest ps1) (conj result (get-value-at b (first ps1))))
                            )))
        same? (fn [values v]
                (= (count values) (count (filter #(= v %) values))))
        get-winner (fn [values]
                     (loop [values1 values]
                       (if (empty? values1)
                         nil  
                         (let [vals (first values1)]
                           (cond
                             (same? vals :x) :x
                             (same? vals :o) :o
                             :else (recur (rest values1)))
                           ))))
        get-values-at-poss (fn [b poss]
                             (loop [result [], poss1 poss]
                               (if (empty? poss1)
                                 result
                                 (let [f (first poss1)
                                       v (get-values-at b f)]
                                   (recur (conj result v) (rest poss1))
                                   ))))
        vals-at-win-poss (get-values-at-poss b win-pos)]
    (get-winner vals-at-win-poss)
    ))

;; Tests

(println (= nil (attt [[:e :e :e]
                       [:e :e :e]
                       [:e :e :e]])))

(println (= :x (attt [[:x :e :o]
                      [:x :e :e]
                      [:x :e :o]])))

(println (= :o (attt [[:e :x :e]
                      [:o :o :o]
                      [:x :e :x]])))

(println (= nil (attt [[:x :e :o]
                       [:x :x :e]
                       [:o :x :o]])))

(println (= :x (attt [[:x :e :e]
                      [:o :x :e]
                      [:o :e :x]])))

(println (= :o (attt [[:x :e :o]
                      [:x :o :e]
                      [:o :e :x]])))

(println (= nil (attt [[:x :o :x]
                       [:x :o :x]
                       [:o :x :o]])))