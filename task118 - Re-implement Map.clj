
;; Re-implement Map

;; Map is one of the core elements of a functional programming language. Given a function f and an input sequence s, return a lazy sequence of (f x) for each element x in s.

(defn mi [f s]
  (lazy-seq
    (if (empty? s)
      ()
      (cons (f (first s))
            (mi f (rest s))))))

;; Tests

(println (= [3 4 5 6 7]
   (mi inc [2 3 4 5 6])))

(println (= (repeat 10 nil)
   (mi (fn [mi] nil) (range 10))))

(println (= [1000000 1000001]
   (->> (mi inc (range))
        (drop (dec 1000000))
        (take 2))))