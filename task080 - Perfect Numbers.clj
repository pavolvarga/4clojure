
;; Perfect Numbers

;; A number is "perfect" if the sum of its divisors equal the number itself. 
;; 6 is a perfect number because 1+2+3=6. 
;; Write a function which returns true for perfect numbers and false otherwise.

(defn perfect-number? [n]
  (let [find-candidates (fn [n]
                          (let [n2 (if (even? n) n (inc n))
                                half (/ n2 2)]
                            (take half (iterate inc 1))))
        candidates (find-candidates n)
        find-divisors (fn [n candidates]
                   (filter #(zero? (rem n %)) candidates))
        divisors (find-divisors n candidates)
        perf-num? (fn [n divisors]
                    (= (reduce + divisors) n))
        
        ]
    (perf-num? n divisors)))

;; Tests

(println (= (perfect-number? 6) true))

(println (= (perfect-number? 7) false))

(println (= (perfect-number? 496) true))

(println (= (perfect-number? 500) false))

(println (= (perfect-number? 8128) true))