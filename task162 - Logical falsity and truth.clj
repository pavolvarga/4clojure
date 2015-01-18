
;; Logical falsity and truth

;; In Clojure, only nil and false represent the values of logical falsity in conditional tests - anything else is logical truth.

(def result 1)

;; Tests

(println (= result (if-not false 1 0)))

(println (= result (if-not nil 1 0)))

(println (= result (if true 1 0)))

(println (= result (if [] 1 0)))

(println (= result (if [0] 1 0)))

(println (= result (if 0 1 0)))

(println (= result (if 1 1 0)))