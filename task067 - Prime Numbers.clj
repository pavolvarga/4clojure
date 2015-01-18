
;; Prime Numbers

;; Write a function which returns the first x number of prime numbers.

(defn get-primes [x]
  (let [is-prime? (fn [x]
                    (loop [n 2]
                      (if (= n x)
                        true
                        (let [r (mod x n)]
                          (if (zero? r)
                            false
                            (recur (inc n)))))))
        ]
    (loop [y 2, result []]
      (if (= x (count result))
        result
        (recur (inc y) (if (is-prime? y) (conj result y) result))
        ))))

;; Tests

(println (= (get-primes 2) [2 3]))

(println (= (get-primes 5) [2 3 5 7 11]))

(println (= (last (get-primes 100)) 541))