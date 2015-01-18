
;; Sequence Reductions

;; Write a function which behaves like reduce, but returns each intermediate value of the reduction. 
;; Your function must accept either two or three arguments, and the return sequence must be lazy.

(defn calc
  ([x y]
   (
    (fn calc2-1 [x y t]
      (lazy-seq
        (cons
          (reduce x t)
          (when-not (empty? y) (calc2-1 x (rest y) (conj t (first y)))))
        ))
    x (rest y) [(first y)]
    )
   )
  ([x y z]
   (
    (fn calc3-1 [x y z t]
      (lazy-seq
        (cons 
          (reduce x y t) 
          (when-not (empty? z) (calc3-1 x y (rest z) (conj t (first z)))))
        )) 
    x y z []
    )
   )
  )

;; Tests

(println (= (take 5 (calc + (range))) [0 1 3 6 10]))

(println (= (calc conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]]))

(println (= (last (calc * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120))