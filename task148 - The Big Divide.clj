
;; The Big Divide

;; Write a function which calculates the sum of all natural numbers under n (first argument) 
;; which are evenly divisible by at least one of a and b (second and third argument). Numbers a and b are guaranteed to be coprimes.
;; Note: Some test cases have a very large n, so the most obvious solution will exceed the time limit.

;;Functional, but slow (last test took 32.4 seconds)
(defn tbd1 [n a b]
  (if (and (< n a) (< n b))
    0
    (loop [sum 0, a1 a, b1 b]
      (cond
        (and (> a1 n)  (> b1 b))        				  sum
        (and (> n a1)  (> n b1) (not (zero? (mod b1 a)))) (recur (+ sum a1 b1) (+ a1 a) (+ b1 b))
        (and (> n a1)  (> n b1) (zero? (mod b1 a))) 	  (recur (+ sum a1)    (+ a1 a) (+ b1 b))
        (and (<= n a1) (> n b1) (not (zero? (mod b1 a)))) (recur (+ sum b1)    a1       (+ b1 b))
        (and (<= n a1) (> n b1) (zero? (mod b1 a)))       (recur sum           a1       (+ b1 b))
        (and (> n a1)  (<= n b1))						  (recur (+ sum a1)    (+ a1 a) b1)
        ))))

;; Accepted solution
;;
;; Based on formula: 
;; n = 100, a = 3, b = 5
;; varX = 90 (highest number evenly divisible by `a` and `b` but less than `n`)
;; varA = 93 + 96 + 99 + 95 (sum of numbers between `x` and `n` divisible by `a` and `b`)
;; varB = ((90 / 3)  x (90 + 3))  / 2
;; varC = ((90 / 5)  x (90 + 5))  / 2
;; varD = ((90 / 15) x (90 + 15)) / 2 (15 = 3 x 5)
;; the result is then (varA + varB + varC - varD)
(defn tbd [n a b]
  (if (and (< n a) (< n b))
    0
    (let [calcX (fn [n a b]
                  (loop [n1 (dec n)]
                    (if (and (zero? (mod n1 a)) (zero? (mod n1 b)))
                      n1
                      (recur (dec n1)))))
          calcA (fn [x n a b]
                  (let [a1 (- (reduce + (range x n a)) x)
                        b1 (- (reduce + (range x n b)) x)]
                    (+ a1 b1)))
          calcBCD (fn [x y] (/ (*' (/ x y) (+ x y)) 2))
          varX (calcX n a b)
          varA (calcA varX n a b)
          varB (calcBCD varX a)
          varC (calcBCD varX b)
          varD (calcBCD varX (* a b))]
      (- (+ varA varB varC) varD))))

;; Tests

(println (= 0 (tbd 3 17 11)))

(println (= 23 (tbd 10 3 5)))

(println (= 233168 (tbd 1000 3 5)))

(println (= "2333333316666668" (str (tbd 100000000 3 5))))

(println (= "110389610389889610389610" (str (tbd (* 10000 10000 10000) 7 11))))

(println (= "1277732511922987429116" (str (tbd (* 10000 10000 10000) 757 809))))

(println (= "4530161696788274281" (str (tbd (* 10000 10000 1000) 1597 3571))))