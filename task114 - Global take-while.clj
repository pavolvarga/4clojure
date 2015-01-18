
;; Global take-while

;; take-while is great for filtering sequences, but it limited: you can only examine a single item of the sequence at a time. 
;; What if you need to keep track of some state as you go over the sequence?
;;
;; Write a function which accepts an integer n, a predicate p, and a sequence. 
;; It should return a lazy sequence of items in the list up to, but not including, the nth item that satisfies the predicate.

(defn gtw [n p s]
  (loop [i 0, result [], s1 s]
    (cond
      (= i n)     (drop-last result)
      (empty? s1) result
      :else
      (let [f (first s1), v (p f)]
        (recur
          (if-not v i (inc i))
          (conj result f)
          (rest s1))
        ))))

;; Tests

(println (= [2 3 5 7 11 13] (gtw 4 #(= 2 (mod % 3)) [2 3 5 7 11 13 17 19 23])))

(println (= ["this" "is" "a" "sentence"] (gtw 3 #(some #{\i} %) ["this" "is" "a" "sentence" "i" "wrote"])))

(println (= ["this" "is"] (gtw 1 #{"a"} ["this" "is" "a" "sentence" "i" "wrote"])))