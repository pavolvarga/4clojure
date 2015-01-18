
;; Universal Computation Engine

;; Given a mathematical formula in prefix notation, return a function that calculates the value of the formula. 
;; The formula can contain nested calculations using the four basic mathematical operators, numeric constants, 
;; and symbols representing variables. The returned function has to accept a single parameter containing 
;; the map of variable names to their values. 

(defn uce [s]
  (fn use-inter [m]
    (let [
          resolve-list (fn [l]
                         (let [o (first l), r (rest l)]
                           (cond 
                             (= '+ o) (apply + r)
                             (= '/ o) (apply / r)
                             (= '- o) (apply - r)
                             (= '* o) (apply * r)
                             )))
          uce-replace (fn uce-replace [s m]
                        (loop [s1 s, result '()]
                          (if (empty? s1)
                            (reverse result)
                            (let [c (first s1)]
                              (recur
                                (rest s1)
                                (cond
                                  (list? c) (conj result (resolve-list (uce-replace c m)))
                                  (contains? m c) (conj result (m c))
                                  :else (conj result c))
                                )))))
          replaced (uce-replace s m)]
      (resolve-list replaced)
      )))

;; Tests

(println (= 2 ((uce '(/ a b)) '{b 8 a 16})) )

(println (= 8 ((uce '(+ a b 2)) '{a 2 b 4})) )

(println (= [6 0 -4] (map (uce '(* (+ 2 a) (- 10 b))) '[{a 1 b 8} {b 5 a -2} {a 2 b 11}])))

(println (= 1 ((uce '(/ (+ x 2) (* 3 (+ y 1)))) '{x 4 y 1})) )