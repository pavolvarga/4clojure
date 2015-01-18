
;; Intro to Destructuring

;; Let bindings and function parameter lists support destructuring.

;; Tests

(println (= [2 4] (let [[a b c d e f g] (range)] (conj [] c e))))