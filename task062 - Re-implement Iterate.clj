
;; Re-implement Iterate

;; Given a side-effect free function f and an initial value x write a function which returns an infinite lazy sequence of x, (f x), (f (f x)), (f (f (f x))), etc.

(defn ii [f x]
  (cons x (lazy-seq (ii f (f x)))))

;; Tests

(println (= (take 5 (ii #(* 2 %) 1)) [1 2 4 8 16]))

(println (= (take 100 (ii inc 0)) (take 100 (range))))

(println (= (take 9 (ii #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3]))))