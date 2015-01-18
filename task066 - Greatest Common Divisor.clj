
;; Greatest Common Divisor

;; Given two integers, write a function which returns the greatest common divisor.

(defn gcd [x y]
  (loop [a x, b y]
    (if (zero? b)
      a
      (recur b (rem a b)))))

;; Tests

(println (= (gcd 2 4) 2))

(println (= (gcd 2 4) 2))

(println (= (gcd 5 7) 1))

(println (= (gcd 1023 858) 33))