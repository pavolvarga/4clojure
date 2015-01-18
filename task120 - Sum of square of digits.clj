
;; Sum of square of digits

;; Write a function which takes a collection of integers as an argument. 
;; Return the count of how many elements are smaller than the sum of their squared component digits. 
;; For example: 10 is larger than 1 squared plus 0 squared; whereas 15 is smaller than 1 squared plus 5 squared.

(defn sd [coll]
  (let [ss (fn [n] (reduce + (map #(Math/pow % 2) (for [x (str n)] (Double/parseDouble (str x))))))
        pred? (fn [x] (< x (ss x)))
       ]
    (count (filter pred? coll))
  ))

;; Tests

(println (= 8 (sd (range 10))))

(println (= 19 (sd (range 30))))

(println (= 50 (sd (range 100))))

(println (= 50 (sd (range 1000))))