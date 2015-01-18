
;; Intro to Functions

;; Clojure has many different ways to create functions.

(def result 8)

;; Tests

(println (= result ((fn add-five [x] (+ x 5)) 3)))

(println (= result ((fn [x] (+ x 5)) 3)))

(println (= result (#(+ % 5) 3)))

(println (= result ((partial + 5) 3)))