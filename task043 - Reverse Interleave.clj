
;; Reverse Interleave

;; Write a function which reverses the interleave process into x number of subsequences.

(defn rev-interleave [s n]
  (let [parts (partition n s)]
    (loop [
           result [], 
           counter (count (first parts)), 
           d parts
           ]
      (if (zero? counter)
        result
        (recur 
          (conj result (map first d))
          (dec counter)
          (map rest d)
          )))))

;; Tests

(println (= (rev-interleave [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6))))

(println (= (rev-interleave (range 9) 3) '((0 3 6) (1 4 7) (2 5 8))))

(println (= (rev-interleave (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9))))