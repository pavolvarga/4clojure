
;; Pascal's Trapezoid

;; Write a function that, for any given input vector of numbers, returns an infinite lazy sequence of vectors, 
;; where each next one is constructed from the previous following the rules used in Pascal's Triangle. 
;; For example, for [3 1 2], the next row is [3 4 3 2].
;;
;; Beware of arithmetic overflow! In clojure (since version 1.3 in 2011), 
;; if you use an arithmetic operator like + and the result is too large to fit into a 64-bit integer, an exception is thrown. 
;; You can use +' to indicate that you would rather overflow into Clojure's slower, arbitrary-precision bigint.

(defn pt [row]
  (let
    [fe (first row)
     le (last row)
     rem-first-last (fn [v] (subvec v 1 (dec (count v))))
     double-members (fn [v] (flatten (for [x v] [x x])))
     put-first-last (fn [v, fe, le] (conj (into [fe] v) le))
     calc 			(fn [v] (map #(reduce +' %) (partition 2 v)))
     calc-next-row  (fn [row] 
						    (cond
						      (= [1]   row) [1 1]
						      (= [1 1] row) [1 2 1]
						      :else
							    (-> row 
               						rem-first-last 
							        double-members 
							        (put-first-last fe le) 
							        calc 
							        (put-first-last fe le))))]
    (iterate calc-next-row row)))

;; Tests

(println (= (second (pt [2 3 2])) [2 5 5 2]))

(println (= (take 5 (pt [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]]))

(println (= (take 2 (pt [3 1 2])) [[3 1 2] [3 4 3 2]]))

(println (= (take 100 (pt [2 4 2])) (rest (take 101 (pt [2 2])))))