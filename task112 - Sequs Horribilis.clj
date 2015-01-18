
;; Sequs Horribilis

;; Create a function which takes an integer and a nested collection of integers as arguments. 
;; Analyze the elements of the input collection and return a sequence which maintains the nested structure, 
;; and which includes all elements starting from the head whose sum is less than or equal to the input integer.

(defn sequs-horribilis [n c]
  (if (true? (instance? clojure.lang.LazySeq c))
    ((fn [n c]
       (loop [sum 0, c1 c, result []]
         (if (<= n (+ sum (first c1)))
           result
           (recur (+ sum (first c1)) (rest c1) (conj result (first c1)))
           )))
     n c)    
    (let [nums (atom [])
          walk1 (fn [x]
                  (if (number? x)
                    (let [sum (reduce + @nums)]
                      (if (<= (+ sum x) n)
                        (swap! nums conj x))))
                  x)
          walk2 (fn walk2 [nums c]
                  (loop [c1 c,  r {:n nums, :d []}]
                    (if (empty? c1)
                      r
                      (let [x (first c1)]
                        (if (number? x)
                          (if (not (empty? (r :n)))
                            (recur 
                              (rest c1) 
                              (assoc r :n (rest (r :n)) :d (conj (r :d) x)))
                            (recur 
                              (rest c1)
                              (assoc r :n (rest (r :n)))))
                          (let [r1 (walk2 (r :n) x) ; inner recursion for inner sequence
                                n1 (r1 :n)
                                d1 (r1 :d)]
                            (recur
                              (rest c1)
                              (assoc r :n n1 :d (if (empty? d1) 
                                                  (r :d) 
                                                  (conj (r :d) (r1 :d))
                                                  ))))
                          )))))
          walk-lazy (fn [n c]
                      (loop [sum 0, c1 c, result []]
                        (if (< n sum)
                          result
                          (recur (+ sum (first c1)) (rest c1) (conj result (first c1)))
                          )))
          _ (clojure.walk/prewalk #(walk1 %) c)]
      ((walk2 @nums c) :d)
      )))

;; Tests

(println (=  (sequs-horribilis 10 [1 2 [3 [4 5] 6] 7]) '(1 2 (3 (4)))))

(println (=  (sequs-horribilis 30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11]) '(1 2 (3 (4 (5 (6 (7))))))))

(println (=  (sequs-horribilis 9 (range)) '(0 1 2 3)))

(println (=  (sequs-horribilis 1 [[[[[1]]]]]) '(((((1)))))))

(println (=  (sequs-horribilis 0 [1 2 [3 [4 5] 6] 7]) '()))

(println (=  (sequs-horribilis 0 [0 0 [0 [0]]]) '(0 0 (0 (0)))))

(println (=  (sequs-horribilis 1 [-10 [1 [2 3 [4 5 [6 7 [8]]]]]]) '(-10 (1 (2 3 (4))))))         