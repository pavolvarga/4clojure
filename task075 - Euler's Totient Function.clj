
;; Euler's Totient Function

;; Two numbers are coprime if their greatest common divisor equals 1. 
;; Euler's totient function f(x) is defined as the number of positive integers less than x which are coprime to x. 
;; The special case f(1) equals 1. Write a function which calculates Euler's totient function.

(defn etf [n]
  (if (= 1 n)
    1
    (let [nums (take (dec n) (iterate inc 1))
          gcd (fn [x y]
                (loop [a x, b y]
                  (if (zero? b)
                    a
                    (recur b (rem a b)))))]
      (count (loop [result [], nums2 nums]
               (if (empty? nums2)
                 result
                 (let [f (first nums2)
                       gcdr (gcd n f)]
                   (when (= 1 gcdr) (conj result gcdr))
                   (recur 
                     (if (= 1 gcdr) (conj result f) result) 
                     (rest nums2))))
               )))))

;; Tests

(println (= (etf 1) 1))

(println (= (etf 10) (count '(1 3 7 9)) 4))

(println (= (etf 40) 16))

(println (= (etf 99) 60))