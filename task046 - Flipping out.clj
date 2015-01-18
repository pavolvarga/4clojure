
;; Flipping out

;; Write a higher-order function which flips the order of the arguments of an input function.

(defn flip-out [f]
  (fn fo [x y] (f y x)))

;; Tests

(println (= 3 ((flip-out nth) 2 [1 2 3 4 5])))

(println (= true ((flip-out >) 7 8)))

(println (= 4 ((flip-out quot) 2 8)))

(println (= [1 2 3] ((flip-out take) [1 2 3 4 5] 3)))