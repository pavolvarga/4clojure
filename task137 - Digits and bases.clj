
;; Digits and bases

;; Write a function which returns a sequence of digits of a non-negative number (first argument) 
;; in numerical system with an arbitrary base (second argument). 
;; Digits should be represented with their integer values, e.g. 15 would be [1 5] in base 10, [1 1 1 1] in base 2 and [15] in base 16. 

(defn get-digits [n radix]
	(if (> radix n)
		[0]
		(loop [x n, result []]
			(if (zero? x)
				(reverse result)
				(recur (quot x radix) (conj result (rem x radix)))))))

;; Tests

(println (= [1 2 3 4 5 0 1] (get-digits 1234501 10)))

(println (= [0] (get-digits 0 11)))

(println (= [1 0 0 1] (get-digits 9 2)))

(println (= [1 0] (let [n (rand-int 100000)](get-digits n n))))

(println (= [16 18 5 24 15 1] (get-digits Integer/MAX_VALUE 42)))