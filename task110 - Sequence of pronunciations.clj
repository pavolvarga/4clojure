
;; Sequence of pronunciations

;; Write a function that returns a lazy sequence of "pronunciations" of a sequence of numbers. 
;; A pronunciation of each element in the sequence consists of the number of repeating identical numbers and the number itself. 
;; For example, [1 1] is pronounced as [2 1] ("two ones"), which in turn is pronounced as [1 2 1 1] ("one two, one one").
;;
;; Your function should accept an initial sequence of numbers, and return an infinite lazy sequence of pronunciations, 
;; each element being a pronunciation of the previous element.

(defn sop [s]
  (let [pronunciate (fn [s]
                      (loop [s1 (partition-by identity s), result []]
                        (if (empty? s1)
                          result
                          (recur
                            (rest s1)
                            (conj result (count (first s1)) (-> s1 first first)))
                          )))]
    (
     (fn lazy-sop [s, l]
       (lazy-seq
         (cons 
           l 
           (lazy-sop (pronunciate s) (pronunciate s))
           )
         ))
     (pronunciate s) (pronunciate s))
    ))

;; Tests

(println (= [[1 1] [2 1] [1 2 1 1]] (take 3 (sop [1]))))

(println (= [3 1 2 4] (first (sop [1 1 1 4 4]))))

(println (= [1 1 1 3 2 1 3 2 1 1] (nth (sop [1]) 6)))

(println (= 338 (count (nth (sop [3 2]) 15))))