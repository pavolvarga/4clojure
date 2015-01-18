
;; Sum It All Up

;; Write a function which returns the sum of a sequence of numbers.

(defn sum-of-seq [s]
  (reduce + s))

;; Tests

(println (= (sum-of-seq [1 2 3]) 6))

(println (= (sum-of-seq (list 0 -2 5 5)) 8))

(println (= (sum-of-seq #{4 2 1}) 7))

(println (= (sum-of-seq '(0 0 -1)) -1))

(println (= (sum-of-seq '(1 10 3)) 14))