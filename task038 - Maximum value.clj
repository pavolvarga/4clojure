
;; Maximum value

;; Write a function which takes a variable number of parameters and returns the maximum value.

(defn get-max [& args]
  ((fn [coll]
     (loop [coll2 coll, result 0]
       (if (empty? coll2)
         result
         (recur 
           (rest coll2) 
           (let [x (first coll2)]
             (if (> x result)
               x
               result)))))) 
   args))

(println (= (get-max 1 8 3 4) 8))

(println (= (get-max 30 20) 30))

(println (= (get-max 45 67 11) 67))