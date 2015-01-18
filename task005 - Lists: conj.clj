
;; Lists: conj

;; When operating on a list, the conj function will return a new list with one or more items "added" to the front.

(def result '(1 2 3 4))

;; Tests

(println (= result (conj '(2 3 4) 1)))

(println (= result (conj '(3 4) 2 1)))