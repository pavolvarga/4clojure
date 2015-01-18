
;; Local bindings

;; Clojure lets you give local names to values using the special let-form.

(def result 7)

;; Tests

(println (= result (let [x 5] (+ 2 x))))

(println (= result (let [x 3, y 10] (- y x))))

(println (= result (let [x 21] (let [y 3] (/ x y)))))