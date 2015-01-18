
;; Oscilrate

;; Write an oscillating iterate: a function that takes an initial value and a variable number of functions. 
;; It should return a lazy sequence of the functions applied to the value in order, restarting from the first function after it hits the end.

(defn oscilrate [n & fns]
  (
   (fn o2 [n f rfuns]
     (lazy-seq
       (let [r (if (nil? f) n (f n))]
         (cons 
           r
           (if (empty? rfuns) 
             (o2 r (first fns) (rest fns))
             (o2 r (first rfuns) (rest rfuns))))
         )))
   n nil fns
   )
  )

;; Tests

(println (= (take 3 (oscilrate 3.14 int double)) [3.14 3 3.0]))

(println (= (take 5 (oscilrate 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7]))

(println (= (take 12 (oscilrate 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3]))