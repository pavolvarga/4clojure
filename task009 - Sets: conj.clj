
;; Sets: conj

;; When operating on a set, the conj function returns a new set with one or more keys "added".

(def result 2)

;; Tests

(println (= #{1 2 3 4} (conj #{1 4 3} 2)))