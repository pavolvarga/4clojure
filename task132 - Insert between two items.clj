
;; Insert between two items

;; Write a function that takes a two-argument predicate, a value, and a collection; 
;; and returns a new collection where the value is inserted between every two items that satisfy the predicate.

(defn ibti [p v c]
  (if (empty? c)
    '()
    (
     (fn lazy-ibti [f c1]
       (lazy-seq
         (cons
           f
           (when-not (empty? c1)
             (cond 
               (= v f) (lazy-ibti (first c1) (rest c1)) 
               (p f (first c1)) (lazy-ibti v c1)
               (not (p f (first c1))) (lazy-ibti (first c1) (rest c1)))
             )))) 
     (first c) (rest c))
    ))

;; Tests

(println (= '(1 :less 6 :less 7 4 3) (ibti < :less [1 6 7 4 3])))

(println (= '(2) (ibti > :more [2])))

(println (= [0 1 :x 2 :x 3 :x 4]  (ibti #(and (pos? %) (< % %2)) :x (range 5))))

(println (empty? (ibti > :more ())))

(println (= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
   (take 12 (->> [0 1]
                 (iterate (fn [[a b]] [b (+ a b)]))
                 (map first) ; fibonacci numbers
                 (ibti (fn [a b] ; both even or both odd
                       (= (mod a 2) (mod b 2)))
                     :same)))))