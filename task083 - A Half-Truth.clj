
;; A Half-Truth

;; Write a function which takes a variable number of booleans. 
;; Your function should return true if some of the parameters are true, but not all of the parameters are true. 
;; Otherwise your function should return false.

(defn half-truth? [& args]
  (let [x (some true? args), y (every? true? args)]
    (if (and (= x true) (= y false))
      true
      false)))

;; Tests

(println (= false (half-truth? false false)))

(println (= true (half-truth? true false)))

(println (= false (half-truth? true)))

(println (= true (half-truth? false true false)))

(println (= false (half-truth? true true true)))

(println (= true (half-truth? true true true false)))