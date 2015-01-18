
;; Least Common Multiple

;; Write a function which calculates the least common multiple. 
;; Your function should accept a variable number of positive integers or ratios. 

(defn varargs-lcm [& args]
	(let [gcd (fn [x y] 
	             (loop [a x b y]
				    (if (zero? b)
				      a
				      (recur b (rem a b)))))
       	  lcm (fn [x y]
                (/ (* x y) (gcd x y)))]
   (loop [acc (first args) v (rest args)]
    (if (empty? v)
      acc
      (recur (lcm acc (first v)) (rest v))))))
   
;; Tests

(println (== (varargs-lcm 2 3) 6))

(println (== (varargs-lcm 5 3 7) 105))

(println (== (varargs-lcm 1/3 2/5) 2))

(println (== (varargs-lcm 3/4 1/6) 3/2))

(println (== (varargs-lcm 7 5/7 2 3/5) 210))