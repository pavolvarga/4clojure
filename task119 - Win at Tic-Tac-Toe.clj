
;; Win at Tic-Tac-Toe

;; As in Problem 73, a tic-tac-toe board is represented by a two dimensional vector. 
;; X is represented by :x, O is represented by :o, and empty is represented by :e. 
;; Create a function that accepts a game piece and board as arguments, and returns a set (possibly empty) of all valid board placements 
;; of the game piece which would result in an immediate win.

(defn wattt [p b]
  (let [win-cords [[[0 0] [0 1] [0 2]]  ; 1st row
                   [[1 0] [1 1] [1 2]]  ; 2nd row
                   [[2 0] [2 1] [2 2]]  ; 3rd row
                   [[0 0] [1 0] [2 0]]  ; 1st column
                   [[0 1] [1 1] [2 1]]  ; 2nd column
                   [[0 2] [1 2] [2 2]]  ; 3rd column
                   [[0 0] [1 1] [2 2]]  ; left to right diagonale
                   [[0 2] [1 1] [2 0]]] ; right to left diagonale
        get-value-at (fn  [b cords] 
                       {:val ((b (cords 0)) (cords 1)), :cords cords})       
        get-values-at (fn  [b ps]
                        (loop [ps1 ps, result []]
                          (if (empty? ps1)
                            result
                            (recur (rest ps1) (conj result (get-value-at b (first ps1))))
                            )))   
        get-values-at-cordss (fn [b cordss]
                               (loop [result [], cords1 cordss]
                                 (if (empty? cords1)
                                   result
                                   (let [f (first cords1)
                                         v (get-values-at b f)]
                                     (recur (conj result v) (rest cords1))
                                     ))))
        winnable? (fn [win-cords p e]
                    (loop [cp 0, ce 0, wc1 win-cords]
                      (if (empty? wc1)
                        (and (= (-> win-cords count dec) cp) (= 1 ce))
                        (let [v ((first wc1) :val)]
                          (cond
                            (= v p) (recur (inc cp) ce (rest wc1))
                            (= v e) (recur cp (inc ce) (rest wc1))
                            :else (recur cp ce (rest wc1)))
                          ))))
        get-empty-cords (fn [cords e]
                          (filter #(= (% :val) e) cords))
        values-at-cordss (get-values-at-cordss b win-cords)
        winnable-cordss (filter #(winnable? % p :e) values-at-cordss)
        empty-cords (map #(get-empty-cords % :e) winnable-cordss)
        empty-cords2 (map #((first %) :cords) empty-cords)
        ]
    (set empty-cords2) 
    ))

;; Tests

(println (= (wattt :x [[:o :e :e] 
                       [:o :x :o] 
                       [:x :x :e]])
            #{[2 2] [0 1] [0 2]}))

(println (= (wattt :x [[:x :o :o] 
                       [:x :x :e] 
                       [:e :o :e]])
            #{[2 2] [1 2] [2 0]}))

(println (= (wattt :x [[:x :e :x] 
                       [:o :x :o] 
                       [:e :o :e]])
            #{[2 2] [0 1] [2 0]}))

(println (= (wattt :x [[:x :x :o] 
                       [:e :e :e] 
                       [:e :e :e]])
            #{}))

(println (= (wattt :o [[:x :x :o] 
                       [:o :e :o] 
                       [:x :e :e]])
            #{[2 2] [1 1]}))