
;; Game of Life

;; The game of life is a cellular automaton devised by mathematician John Conway. 
;;
;; The 'board' consists of both live (#) and dead ( ) cells. 
;; Each cell interacts with its eight neighbours (horizontal, vertical, diagonal), 
;; and its next state is dependent on the following rules:
;;
;; 1) Any live cell with fewer than two live neighbours dies, as if caused by under-population.
;; 2) Any live cell with two or three live neighbours lives on to the next generation.
;; 3) Any live cell with more than three live neighbours dies, as if by overcrowding.
;; 4) Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
;;
;; Write a function that accepts a board, and returns a board representing the next generation of cells.

(defn next-gen-gof [board]
	(let [	live-cell \#
		  	dead-cell \space
		  	convert-to-vectors 		(fn [board]
										"Converts vector of strings into vector of vectors of characters"
										(loop [b1 board, result []]
											(if (empty? b1)
												result
												(recur (rest b1) (conj result (vec (first b1))))
											)))
		  	gen-indexes 			(fn [rows columns]
										"Generate map of indexes"
										(for [i (range 0 rows), j (range 0 columns)]
											{:row i, :col j}
											))
		  	gen-idxs-of-neighbours 	(fn [idx rows columns]
										"Return indexes of 8 neighbours, filter out indexes outside of the board"
										(let [	{r :row, c :col} idx
												dr (dec r) 
												dc (dec c) 
												ir (inc r) 
												ic (inc c)
												r1 [{:row dr, :col dc}, {:row dr, :col c}, {:row dr, :col ic},
											    	{:row r,  :col dc},                  , {:row r , :col ic},
											    	{:row ir, :col dc}, {:row ir, :col c}, {:row ir, :col ic}]
												pred (fn [{r :row, c :col}]
														(cond
															(neg? r) 			false
															(neg? c) 			false
															(> r (dec rows)) 	false
															(> c (dec columns)) false
															:default true))]
											    (filter pred r1)
											))
			is-live? 			 	(fn [c]
										(if (= live-cell c) true false))
			get-vals				(fn [board, indexes]
										"Get values from the board at indexes"
										(loop [idx1 indexes, result []]
											(if (empty? idx1)
												result
												(let [{r :row, c :col} (first idx1)
													  v ((board r) c)]
													  (recur (rest idx1) (conj result v))
													))))
			how-many-is-alive		(fn [neighbours]
										"Return count of live cells from input"
											(count (filter is-live? neighbours)))
			evolve-for-live 		(fn [neighbours]
										"Depending on number of live neighbours evolve actual live-cell according to rules"
										(let [alive (how-many-is-alive neighbours)]
											(cond
												(< alive 2) 				 dead-cell
												(or (= alive 2) (= alive 3)) live-cell
												(> alive 3)					 dead-cell)
										))
			evolve-for-dead 		(fn [neighbours]
										"Depending on number of dead neighbours evolve actual dead-cell according to rules"
										(let [alive (how-many-is-alive neighbours)]
											(if (= alive 3)
												live-cell
												dead-cell)
											))
			evolve-cell 			(fn [cell neighbours]
										"Evolve the cell depening on type and number of neighbours according to rules"
										(if (is-live? cell)
											(evolve-for-live neighbours)
											(evolve-for-dead neighbours)
											))
			evolve-board			(fn [board]
										(let [	vectorized-board	(convert-to-vectors board)
												rows				(count vectorized-board)
												columns				(count (first vectorized-board))
												indexes 			(gen-indexes rows columns)]
												(loop [idxs1 indexes, result []]
													(if (empty? idxs1)
														result
														(let [idx 				(first idxs1)
															  r 				(idx :row)
															  c 				(idx :col)
															  cell 				((vectorized-board r) c)
															  idxs-of-neighs 	(gen-idxs-of-neighbours idx rows columns)
															  neighbours 		(get-vals vectorized-board idxs-of-neighs)
															  next-cell			(evolve-cell cell neighbours)]
															(recur (rest idxs1) (conj result next-cell)))
														)))) 
			next-board				(evolve-board board)
			partitioned-board		(partition (-> board first count) next-board)]
		(map #(apply str %) partitioned-board)
))

;; Tests

(println (= (next-gen-gof [ "      "  
					        " ##   "
					        " ##   "
					        "   ## "
					        "   ## "
					        "      "])
								   ["      "  
								    " ##   "
								    " #    "
								    "    # "
								    "   ## "
								    "      "]))
(println (= (next-gen-gof [ "     "
					        "     "
					        " ### "
					        "     "
					        "     "])
								   ["     "
								    "  #  "
								    "  #  "
								    "  #  "
								    "     "]))
(println (= (next-gen-gof [ "      "
					        "      "
					        "  ### "
					        " ###  "
					        "      "
					        "      "])
								   ["      "
								    "   #  "
								    " #  # "
								    " #  # "
								    "  #   "
								    "      "]))