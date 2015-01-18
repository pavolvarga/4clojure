
;; Contain Yourself

;; The contains? function checks if a KEY is present in a given collection. 
;; This often leads beginner clojurians to use it incorrectly with numerically indexed collections like vectors and lists.

(def result 4)

;; Tests

(println (contains? #{4 5 6} result))

(println (contains? [1 1 1 1 1] result))

(println (contains? {4 :a 2 :b} result))

;; will not work
; (println (not (contains? '(1 2 4) result)))