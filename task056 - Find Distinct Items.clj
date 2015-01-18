
;; Find Distinct Items

;; Write a function which removes the duplicates from a sequence. Order of the items must be maintained.

(defn own-distinct [v]
  (loop [result [(first v)], input (rest v)]
   (if (empty? input)
     result
     (let [elem (first input)
           is-in-r (.indexOf result elem)]
     (recur 
       (if (= -1 is-in-r) (conj result elem) result) 
       (rest input))
     ))))

;; Tests

(println (= (own-distinct [1 2 1 3 1 2 4]) [1 2 3 4]))

(println (= (own-distinct [:a :a :b :b :c :c]) [:a :b :c]))

(println (= (own-distinct '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3])))

(println (= (own-distinct (range 50)) (range 50)))