
;; Decurry

;; Write a function that accepts a curried function of unknown arity n. Return an equivalent function of n arguments. 

; Works in a specific case - that arity of each function will be always 1
; accepted solution
(defn decurry1 [f]
  (fn [& args]
    (loop [f1 f, args1 args]
      (let [v (f1 (first args1))]
        (if-not (fn? v)
          v
          (recur v (rest args1))
          )))))

; Works in all cases, but was not accepted
(defn decurry [f]
  (let [arg-count (fn [f]
                    (let [m (first (.getDeclaredMethods (class f)))
                          p (.getParameterTypes m)]
                      (alength p)))]
    (fn [& args]
      (loop [f1 f, args1 args]
        (let [a (arg-count f1)
              v (if (= 1 a) (f1 (first args1)) (apply f1 (drop args1 a)))]
          (if-not (fn? v)
            v
            (recur v (nthnext args1 a))
            ))))))

;; Tests

(println (= 10 ((decurry (fn [a]
             (fn [b]
               (fn [c]
                 (fn [d]
                   (+ a b c d))))))
       1 2 3 4)))

(println (= 24 ((decurry (fn [a]
             (fn [b]
               (fn [c]
                 (fn [d]
                   (* a b c d))))))
       1 2 3 4)))

(println (= 25 ((decurry (fn [a]
             (fn [b]
               (* a b))))
       5 5)))
