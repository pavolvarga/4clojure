
;; Reimplement Trampoline

;; Reimplement the function described in "Intro to Trampoline".

(defn own-trampoline [f & args]
  (loop [v (if (empty? args) (f) (apply f args))]
    (if-not (fn? v)
      v
      (recur (v))
      )))

;; Tests

(println (= (letfn [(triple [x] #(sub-two (* 3 x)))
                    (sub-two [x] #(stop?(- x 2)))
                    (stop? [x] (if (> x 50) x #(triple x)))]
              (own-trampoline triple 2))
            82))

(println (= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
                    (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
              (map (partial own-trampoline my-even?) (range 6)))
            [true false true false true false]))