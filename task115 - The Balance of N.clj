
;; The Balance of N

;; A balanced number is one whose component digits have the same sum on the left and right halves of the number. 
;; Write a function which accepts an integer n, and returns true iff n is balanced.

(defn balanced? [n]
  (let [split-num (fn [n]
                    (let [s (str n)
                          c (count s)]
                      (if (even? c)
                        (let [h (/ c 2)]
                          (conj [] (subs s 0 h) (subs s h)))
                        (let [h (quot c 2)]
                          (conj [] (subs s 0 h) (subs s (inc h))))
                        ))) 
        calc-sum-digs (fn [s]
                        (let [chs (seq s)
                              nums (map #(-> % str Integer/parseInt) chs)]
                          (reduce + nums)))
        halfs (split-num n)]
    (= (-> (first halfs) calc-sum-digs) (-> (last halfs) calc-sum-digs))
    ))

;; Tests

(println (= true (balanced? 11)))

(println (= true (balanced? 121)))

(println (= false (balanced? 123)))

(println (= true (balanced? 0)))

(println (= false (balanced? 88099)))

(println (= true (balanced? 89098)))

(println (= true (balanced? 89089)))

(println (= (take 20 (filter balanced? (range)))
   [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101]))