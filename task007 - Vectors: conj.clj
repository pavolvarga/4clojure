
;; Vectors: conj

;; When operating on a Vector, the conj function will return a new vector with one or more items "added" to the end.

(def result [1 2 3 4])

;; Tests

(println (= result (conj [1 2 3] 4)))

(println (= result (conj [1 2] 3 4)))