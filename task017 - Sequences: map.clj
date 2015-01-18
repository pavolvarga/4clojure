
;; Sequences: map

;; The map function takes two arguments: a function (f) and a sequence (s). 
;; Map returns a new sequence consisting of the result of applying f to each item of s. 
;; Do not confuse the map function with the map data structure.

(def result '(6 7 8))

;; Tests

(println (= result (map #(+ % 5) '(1 2 3))))