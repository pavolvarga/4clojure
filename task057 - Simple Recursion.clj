
;; Simple Recursion

;; A recursive function is a function which calls itself. This is one of the fundamental techniques used in functional programming.

(def result '(5 4 3 2 1))

;; Tests

(println (= result ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5)))