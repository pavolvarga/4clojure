
;; Comparisons

;; For any orderable data type it's possible to derive all of the basic comparison operations (<, ≤, =, ≠, ≥, and >) 
;; from a single operation (any operator but = or ≠ will work). 
;; Write a function that takes three arguments, a less than operator for the data and two items to compare. 
;; The function should return a keyword describing the relationship between the two items. 
;; The keywords for the relationship between x and y are as follows:
;; 		x = y → :eq
;; 		x > y → :gt
;; 		x < y → :lt

(defn cp [f x y]
    (if (f x y)
      :lt
      (if (f y x)
        :gt
        :eq)))

;; Tests

(println (= :gt (cp < 5 1)))

(println (= :eq (cp (fn [x y] (< (count x) (count y))) "pear" "plum")))

(println (= :lt (cp (fn [x y] (< (mod x 5) (mod y 5))) 21 3)))

(println (= :gt (cp > 0 2)))