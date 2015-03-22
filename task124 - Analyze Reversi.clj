
;; Analyze Reversi
;;
;; Reversi is normally played on an 8 by 8 board. In this problem, a 4 by 4 board is represented as a 
;; two-dimensional vector with black, white, and empty pieces represented by 'b, 'w, and 'e, respectively. 
;; Create a function that accepts a game board and color as arguments, and returns a map of legal moves for that color. 
;; Each key should be the coordinates of a legal move, and its value a set of the coordinates of the pieces flipped by that move.
;;
;; Board coordinates should be as in calls to get-in. For example, [0 1] is the topmost row, second column from the left.

; movements [0, 1, 2, 3, 4, 5, 6, 7]: 
; [left-top-diagonal,    top-vertical,    right-top-diagonal, 
;  left-horizontal,                       right-horizontal, 
;  left-bottom-diagonal, bottom-vertical, right-bottom-diagonal]
(defn arb [board color]
  (letfn [
    ; return positions for all pieces in the `board` with `color`
    (get-positions [board color]
      (let [get-pos-row (fn [row color] 
                          (map #(first %) 
                            (filter #(= color (second %)) 
                              (map-indexed (fn [idx itm] [idx itm]) row))))
            idx-rows (map-indexed (fn [idx itm] [idx (get-pos-row itm color)]) board)
            filt-rows (filter #(not (empty? (second %))) idx-rows)
            positions (map (fn [el]  (for [x [(first el)] y (second el)] [x y])) filt-rows)]
      (vec (partition 2 2 (flatten positions)))))
    ; is `pos` inside of a board
    (is-inside?   [pos rows cols] 
                    (cond 
                      (neg? (first pos))     false
                      (= (first pos) rows)   false
                      (neg? (second pos))    false
                      (= (second pos) cols)  false
                      :else                  true))
    ; return positions of 8 neighbours (diagonali, horizontaly and verticaly)
    ; if any position is outside a board, then nil is used instead of a position
    (get-neigh-pos [pos rows cols]
      (let [[r c] pos
            rd (dec r)
            ri (inc r)
            cd (dec c)
            ci (inc c)
            cands [[rd cd] [rd c] [rd ci] [r cd] [r ci] [ri cd] [ri c] [ri ci]]]
              (map #(if (is-inside? % rows cols) % nil) cands)))
    ; return piece from `board` at `pos`
    (get-at [board pos] (if (nil? pos) nil (get-in board pos)))
    ; return neighbours pieces for `pos`
    ; nil is used if a neighbour is outside the `board`
    (get-neighbours [board pos rows cols]
      (map #(get-at board %) (get-neigh-pos pos rows cols)))
    ; get opposite color to input `color`
    (opp-color [color] (if (= 'w color) 'b 'w))
    ; return list of movements (for example 4 is move right horizontali)
    (get-movements [neighbours color]
      (filter 
        #(number? %) 
        (map-indexed (fn [idx itm] (when (= itm color) idx)) neighbours)))
    ; get next position by using movement of `move-type`
    (next-pos [pos move-type]
      (let [[r c] pos]
        (cond
          (= 0 move-type) [(dec r) (dec c)]
          (= 1 move-type) [(dec r) c]
          (= 2 move-type) [(dec r) (inc c)]
          (= 3 move-type) [r       (dec c)]
          (= 4 move-type) [r       (inc c)]
          (= 5 move-type) [(inc r) (dec c)]
          (= 6 move-type) [(inc r) c]
          (= 7 move-type) [(inc r) (inc c)]
          :else nil)))
    ; get positions by using movement of `move-type` until the board edge
    ; starting from `start` (exclusive)
    (next-positions [start move-type rows cols]
      (loop [pos start result []]
        (let [npos (next-pos pos move-type)]
          (if (is-inside? npos rows cols)
            (recur npos (conj result npos))
            result))))
    ; convert a line to nill if the end is not an empty piece
    ; otherwise drop all empty pieces to one
    ; '((b b) (b b e ) (b b b) (b e e e e)) -> '(nil (b b e) nill (b e))
    (process-lines [lines]
      (letfn [(cut-empty-pieces 
                [line] 
                (conj (vec (take-while #(not (= 'e %)) line)) 'e))]
        (map
          (fn [line]
            (if (not (= 'e (last line))) nil (cut-empty-pieces line)))
          lines)))
    ; examples of inputs:
    ; ml-pieces:    '[[b e] [b e] [b e] [b e]]
    ; ml-positions: '[[[1 2] [1 3]] [[2 1] [3 1]] [[1 2] [0 2]] [[2 1] [2 0]]]
    ; from these inputs create a map of possible moves in requested format
    (create-moves [ml-pieces ml-positions]
      (loop [pieces ml-pieces i 0 result {}]
        (if (empty? pieces)
          result
          (let [line (first pieces)
                positions (ml-positions i)]
            (if (nil? line)
              (recur (rest pieces) (inc i) result)
              (recur (rest pieces) (inc i) (assoc result (last positions) (set (drop-last positions)))))))))]
    (let [rows (count board)
          cols (count (first board))
          opponent-color (opp-color color)
          positions (get-positions board color)
          movements (map 
                      (fn [el] 
                        (let [neighbours (get-neighbours board el rows cols)]
                          (get-movements neighbours opponent-color))) 
                      positions)
          move-lines-pos1  (map-indexed
                            (fn [idx itm]
                              (let [pos (positions idx)]
                                (map #(next-positions pos % rows cols) itm)))
                            movements)
          move-lines-pos2 (vec (mapcat identity move-lines-pos1))
          move-lines-pieces1 (map (fn [el] (map #(get-at board %) el)) move-lines-pos2)
          move-lines-pieces2 (vec (process-lines move-lines-pieces1))]
      (create-moves move-lines-pieces2 move-lines-pos2))))

;; Tests

(println (={[1 3] #{[1 2]}, [0 2] #{[1 2]}, [3 1] #{[2 1]}, [2 0] #{[2 1]}}
   (arb '[[e e e e]
          [e w b e]
          [e b w e]
          [e e e e]] 'w)))

(println (={[3 2] #{[2 2]}, [3 0] #{[2 1]}, [1 0] #{[1 1]}}
   (arb '[[e e e e]
          [e w b e]
          [w w w e]
          [e e e e]] 'b)))

(println (={[0 3] #{[1 2]}, [1 3] #{[1 2]}, [3 3] #{[2 2]}, [2 3] #{[2 2]}}
   (arb '[[e e e e]
          [e w b e]
          [w w b e]
          [e e b e]] 'w)))

(println (={[0 3] #{[2 1] [1 2]}, [1 3] #{[1 2]}, [2 3] #{[2 1] [2 2]}}
   (arb '[[e e w e]
          [b b w e]
          [b w w e]
          [b w w w]] 'b)))