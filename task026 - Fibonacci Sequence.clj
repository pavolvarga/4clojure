
;; Fibonacci Sequence

;; Write a function which returns the first X fibonacci numbers.

(defn fibo [n] 
  (rest 
    ((fn [n] 
       (loop [x 0, result []]
         (if (> x n)
           result
           (recur 
             (+ x 1) 
             (conj result (cond
                            (= x 0) 0
                            (= x 1) 1
                            :else (let [a (last result), b (first (take-last 2 result))] (+ a b))
                            )))
           ))) 
     n)))

;; Tests

(println (= (fibo 3) '(1 1 2)))

(println (= (fibo 6) '(1 1 2 3 5 8)))

(println (= (fibo 8) '(1 1 2 3 5 8 13 21)))