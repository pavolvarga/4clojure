
;; Advanced Destructuring

;; Here is an example of some more sophisticated destructuring.

(def result '(1 2 3 4 5))

;; Tests

(println (= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] result] [a b c d])))