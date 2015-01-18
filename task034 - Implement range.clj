
;; Implement range

;; Write a function which creates a list of all integers in a given range.

(defn cr [s e] 
  (reverse (loop [a s, result ()]
    (if (= a e)
      result
      (recur (inc a) (conj result a))))))

;; Tests

(println (= (cr 1 4) '(1 2 3)))

(println (= (cr -2 2) '(-2 -1 0 1)))

(println (= (cr 5 8) '(5 6 7)))