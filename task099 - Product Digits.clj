
;; Product Digits

;; Write a function which multiplies two numbers and returns the result as a sequence of its digits.

(defn product-digits [a b] 
  (map 
    #(Integer/parseInt (str %)) 
    (seq (str (* a b)))))

;; Tests

(println (= (product-digits 1 1) [1]))

(println (= (product-digits 99 9) [8 9 1]))

(println (= (product-digits 999 99) [9 8 9 0 1]))