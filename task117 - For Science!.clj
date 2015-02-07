
;; For Science!
;;
;; A mad scientist with tenure has created an experiment tracking mice in a maze. 
;; Several mazes have been randomly generated, and you've been tasked with writing a program 
;; to determine the mazes in which it's possible for the mouse to reach the cheesy endpoint. 
;; Write a function which accepts a maze in the form of a collection of rows, each row is a string where: 
;;
;;  - spaces represent areas where the mouse can walk freely
;;  - hashes (#) represent walls where the mouse can not walk
;;  - M represents the mouse's starting point
;;  - C represents the cheese which the mouse must reach
;;
;; The mouse is not allowed to travel diagonally in the maze (only up/down/left/right), 
;; nor can he escape the edge of the maze. Your function must return true iff the maze is solvable by the mouse.

(defn is-reachable? [maze]
  (let [rows (count maze)
        cols (count (first maze))
        init-maze (fn [maze]
                    (letfn [
                      (decide-type [p]
                        (cond
                          (= p \space) :space
                          (= p \#)     :wall
                          (= p \C)     :cheese
                          :else        :mouse))
                      (init-row [row idx]
                        (loop [r (vec row), i 0, result []]
                          (if (empty? r)
                            result
                            (recur 
                              (vec (rest r))
                              (inc i) 
                              (conj result {:row idx, :col i, :type (decide-type (first r)) })))))]
                    (loop [rows maze, i 0, result []]
                      (if (empty? rows)
                        result
                        (recur
                          (vec (rest rows))
                          (inc i)
                          (into result (init-row (first rows) i)))))))
        in-maze (init-maze maze)
        mouse (first (filter #(= :mouse (% :type)) in-maze))
        calc-moves  (fn calc-moves [row col]
                      (let [candidates  [[row (dec col)]  ; left
                                         [row (inc col)]  ; right
                                         [(dec row) col]  ; down
                                         [(inc row) col]] ; up
                            is-inside? (fn [p]
                                          (let [[row col] p]
                                            (not
                                              (cond
                                                (or (neg? row) (neg? col))  true
                                                (> row (dec rows))          true
                                                (> col (dec cols))          true
                                                :else                       false))))]
                      (filter is-inside? candidates)))
        find-cell (fn [pos]
                    (first (filter #(and (= (% :row) (pos 0)) (= (% :col) (pos 1))) in-maze)))
        is-cell-a?  (fn [cell type]
                      (= type (cell :type)))
        is-wall?    (fn [cell]
                      (is-cell-a? cell :wall))
        is-space?   (fn [cell]
                      (is-cell-a? cell :space))
        is-cheese?  (fn [cell]
                      (is-cell-a? cell :cheese))
        is-mouse?  (fn [cell]
                      (is-cell-a? cell :mouse))
        get-open-cells  (fn [cell]
                          (let [row (cell :row)
                                col (cell :col)
                                moves (calc-moves row col)
                                cells (map find-cell moves)]
                            (filter #(or (is-space? %) (is-mouse? %) (is-cheese? %)) cells)))
        cell-to-pos (fn [cell]
                      (vector (cell :row) (cell :col)))
        get-open-cells2 (fn [pos]
                          (get-open-cells (find-cell pos)))
        remove-visited  (fn [candidates visited]
                          (remove (fn [p] (not (empty? (filter #(= p %) visited)))) candidates))
        exists-path?  (fn [start-pos]
                        (loop [visited [start-pos], move (get-open-cells2 start-pos)]
                          (if (empty? move)
                            false
                            (let [actual (first move)
                                  other  (rest move)
                                  movable (get-open-cells actual)
                                  open-cells (distinct (into other movable))
                                  new-move (remove-visited open-cells visited)]
                              (if (is-cheese? actual)
                                true
                                (recur (conj visited actual) new-move))))))]
    (exists-path? (cell-to-pos mouse))))

;; Tests

(println (= true  (is-reachable? ["M   C"])))

(println (= false (is-reachable? ["M # C"])))

(println (= true  (is-reachable?   ["#######"
                                    "#     #"
                                    "#  #  #"
                                    "#M # C#"
                                    "#######"])))

(println (= false (is-reachable?   ["########"
                                    "#M  #  #"
                                    "#   #  #"
                                    "# # #  #"
                                    "#   #  #"
                                    "#  #   #"
                                    "#  # # #"
                                    "#  #   #"
                                    "#  #  C#"
                                    "########"])))

(println (= false (is-reachable?   ["M     "
                                    "      "
                                    "      "
                                    "      "
                                    "    ##"
                                    "    #C"])))

(println (= true  (is-reachable?   ["C######"
                                    " #     "
                                    " #   # "
                                    " #   #M"
                                    "     # "])))

(println (= true  (is-reachable?   ["C# # # #"
                                    "        "
                                    "# # # # "
                                    "        "
                                    " # # # #"
                                    "        "
                                    "# # # #M"])))