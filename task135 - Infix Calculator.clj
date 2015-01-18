
;; Infix Calculator

;; Your friend Joe is always whining about Lisps using the prefix notation for math. 
;; Show him how you could easily write a function that does math using the infix notation. 
;; Is your favorite language that flexible, Joe? 
;; Write a function that accepts a variable length mathematical expression consisting of numbers and the operations +, -, *, and /. 
;; Assume a simple calculator that does not do precedence and instead just calculates left to right.

(defn inf [& coll]
  (loop [s (rest coll), result (first coll)]
    (if (empty? s)
      result
      (recur (drop 2 s) ((first s) result (nth s 1)))
    )))

;; Tests

(println (= 7  (inf 2 + 5)))

(println (= 42 (inf 38 + 48 - 2 / 2)))

(println (= 8  (inf 10 / 2 - 1 * 2)))

(println (= 72 (inf 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9)))