
;; Juxtaposition

;; Take a set of functions and return a new function that takes a variable number 
;; of arguments and returns a sequence containing the result of applying each function 
;; left-to-right to the argument list.

(defn own-juxt [& fns]
  (fn
    [& args]
    (loop [result [], funs fns]
      (if (empty? funs)
        result
      	(recur (conj result (apply (first funs) args)) (rest funs))))))

;; Tests

(println (= [21 6 1] ((own-juxt + max min) 2 3 5 1 6 4)))

(println (= ["HELLO" 5] ((own-juxt #(.toUpperCase %) count) "hello")))

(println (= [2 6 4] ((own-juxt :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10})))