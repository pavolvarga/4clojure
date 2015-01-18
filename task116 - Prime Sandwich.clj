
;; Prime Sandwich

;; A balanced prime is a prime number which is also the mean of the primes directly before and after it in the sequence of valid primes. 
;; Create a function which takes an integer n, and returns true iff it is a balanced prime.

(defn balanced-prime? [x]
  (let [is-prime? (fn [x]
                    (cond
                      (<= x 1) false
                      (= x 2) true
                      :else
                      (loop [n 2]
                        (if (= n x)
                          true
                          (let [r (mod x n)]
                            (if (zero? r)
                              false
                              (recur (inc n))))))))
        get-next-prime (fn [x]
                         (loop [n (inc x)]
                           (if (is-prime? n)
                             n
                             (recur (inc n)))))
        get-prev-prime (fn [x] 
                         (if (<= x 2) 
                           0
                           (loop [n (dec x)]
                             (if (is-prime? n)
                               n
                               (recur (dec n))))))
        next-prime (get-next-prime x)
        prev-prime (get-prev-prime x)]
    (if (is-prime? x)
      (= x (/ (+ next-prime prev-prime) 2))
      false
      )))

;; Tests

(println (= false (balanced-prime? 4)))

(println (= true (balanced-prime? 563)))

(println (= 1103 (nth (filter balanced-prime? (range)) 15)))