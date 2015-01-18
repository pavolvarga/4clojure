
;; Intervals

;; Write a function that takes a sequence of integers and returns a sequence of "intervals". 
;; Each interval is a a vector of two integers, start and end, 
;; such that all integers between start and end (inclusive) are contained in the input sequence.

(defn intervals [s]
  (let [
        mark-intervals (fn [s1]
                         (loop [s2 (rest s1), result [(first s1)]]
                           (if (empty? s2)
                             result
                             (let [f (first s2)]
                               (if (= f (inc (last result)))
                                 (recur (rest s2) (conj result f))
                                 (recur (rest s2) (conj result :b f))
                                 )
                               ))
                           ))
        create-intervals (fn [s]
                           (loop [s1 s, result []]
                             (if (empty? s1)
                               result
                               (let [ins (first s1)
                                     f (first ins)
                                     l (last ins)]
                                 (recur (rest s1) (conj result [f l]))
                                 ))))
        s1 (-> s sort distinct)
        s2 (mark-intervals s1)
        s3 (partition-by keyword? s2)
        s4 (filter #(number? (first %)) s3)
        s5 (create-intervals s4)]
    s5))

;; Tests

(println (= (intervals [1 2 3]) [[1 3]]))

(println (= (intervals [10 9 8 1 2 3]) [[1 3] [8 10]]))

(println (= (intervals [1 1 1 1 1 1 1]) [[1 1]]))

(println (= (intervals []) []))

(println (= (intervals [19 4 17 1 3 10 2 13 13 2 16 4 2 15 13 9 6 14 2 11])
       [[1 4] [6 6] [9 11] [13 17] [19 19]]))