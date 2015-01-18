
;; Function Composition

;; Write a function which allows you to create function compositions. 
;; The parameter list should take a variable number of functions, and create a function applies them from right-to-left.

(defn comp-r-l [& args]
  (fn 
    [input]
    (loop [result input, fns (reverse args)]
      (if (empty? fns)
        result
        (recur ((first fns) result) (rest fns)))))
  (fn 
    [a & i]
    (let [input (conj i a)
          funs (reverse args)]
      (loop [result (apply (first funs) input), fns (rest funs)]
        (if (empty? fns)
          result
          (recur ((first fns) result) (rest fns))))))
  )

;; Tests

(println (=  [3 2 1] ((comp-r-l rest reverse) [1 2 3 4])))

(println (=  5 ((comp-r-l (partial + 3) second) [1 2 3 4])))

(println (=  true ((comp-r-l zero? #(mod % 8) +) 3 5 7 9)))

(println (=  "HELLO" ((comp-r-l #(.toUpperCase %) #(apply str %) take) 5 "hello world")))