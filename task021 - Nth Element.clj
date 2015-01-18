
;; Nth Element

;; Write a function which returns the Nth element from a sequence.

(defn get-nth [s n] 
  (last (take (+ 1 n) s)))

;; Tests

(println (= (get-nth '(4 5 6 7) 2) 6))

(println (= (get-nth [:a :b :c] 0) :a))

(println (= (get-nth [1 2 3 4] 1) 2))

(println (= (get-nth '([1 2] [3 4] [5 6]) 2) [5 6]))