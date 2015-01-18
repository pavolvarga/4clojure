
;; Intro to Destructuring 2

;; Sequential destructuring allows you to bind symbols to parts of sequential things (vectors, lists, seqs, etc.): 
;; (let [bindings* ] exprs*) Complete the bindings so all let-parts evaluate to 3.

;; Tests

(println (= 3
  (let [[a c] [+ (range 3)]] (apply a c))
  (let [[[a c] b] [[+ 1] 2]] (a c b))
  (let [[a c] [inc 2]] (a c))))