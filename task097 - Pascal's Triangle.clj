
;; Pascal's Triangle

;; Pascal's triangle is a triangle of numbers computed using the following rules:
;;
;;  - The first row is 1.
;;  - Each successive row is computed by adding together adjacent numbers in the row above, and adding a 1 to the beginning and end of the row.
;;
;; Write a function which returns the nth row of Pascal's Triangle. 

(defn pasc-tree [n]
  (let 
    [rem-first-last (fn [v]  (subvec v 1 (dec (count v))))
     double-members (fn [v]  (flatten (for [x v] [x x])))
     put-first-last (fn [v]  (conj (into [1] v) 1))
     calc           (fn [v]  (map #(reduce + %) (partition 2 v)))
     calc-row (fn [prev-row] 
               (-> prev-row rem-first-last double-members put-first-last calc put-first-last))]
  (cond
    (= 1 n) [1]
    (= 2 n) [1 1]
    (= 3 n) [1 2 1]
      :else     
      (loop [act 4 row [1 2 1]]
        (if (> act n)
          row
          (recur (inc act) (calc-row row)))))))
  
;; Tests

(println (= (pasc-tree 1) [1]))

(println (= (map pasc-tree (range 1 6))
   [     [1]
        [1 1]
       [1 2 1]
      [1 3 3 1]
     [1 4 6 4 1]]))

(println (= (pasc-tree 11)
   [1 10 45 120 210 252 210 120 45 10 1]))