
;; Factorial Fun

;; Write a function which calculates factorials.

(defn fac [n]
  (loop [x n, result 1]
    (if (= x 0)
      result
      (recur (dec x) (* result x)))))

;; Tests

(println (= (fac 1) 1))

(println (= (fac 3) 6))

(println (= (fac 5) 120))

(println (= (fac 8) 40320))