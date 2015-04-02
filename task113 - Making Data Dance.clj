
;; Making Data Dance
;;
;; Write a function that takes a variable number of integer arguments. 
;; If the output is coerced into a string, it should return a comma (and space) separated list of the inputs sorted smallest to largest. 
;; If the output is coerced into a sequence, it should return a seq of unique input elements in the same order as they were entered.

(defn process-ints [& args]
  (reify 
    java.lang.Object      (toString [this] (apply str (interpose ", " (sort args))))
    clojure.lang.Seqable  (seq [this] (keys (group-by identity args)))))

;; Tests

(println (= "1, 2, 3" (str (process-ints 2 1 3))))

(println (= '(2 1 3) (seq (process-ints 2 1 3))))

(println (= '(2 1 3) (seq (process-ints 2 1 3 3 1 2))))

(println (= '(1) (seq (apply process-ints (repeat 5 1)))))

(println (= "1, 1, 1, 1, 1" (str (apply process-ints (repeat 5 1)))))

(println (and (= nil (seq (process-ints)))
              (=  "" (str (process-ints)))))