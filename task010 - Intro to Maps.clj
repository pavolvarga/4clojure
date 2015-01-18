
;; Intro to Maps

;; Maps store key-value pairs. Both maps and keywords can be used as lookup functions. 
;; Commas can be used to make maps more readable, but they are not required.

(def result 20)

;; Tests

(println (= result ((hash-map :a 10, :b 20, :c 30) :b)))

(println (= result (:b {:a 10, :b 20, :c 30})))