
;; Find the odd numbers

;; Write a function which returns only the odd numbers from a sequence.

(defn find-odd-nums [s]
  (filter odd? s))

;; Tests

(println (= (find-odd-nums #{1 2 3 4 5}) '(1 3 5)))

(println (= (find-odd-nums [4 2 1 6]) '(1)))

(println (= (find-odd-nums [2 2 4 6]) '()))

(println (= (find-odd-nums [1 1 1 3]) '(1 1 1 3)))