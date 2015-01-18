
;; Rotate Sequence

;; Write a function which can rotate a sequence in either direction.

(defn rot-seq [n s]
  (let [
       s2 (if (neg? n) (reverse s) s)
       n2 (if (neg? n) (* -1 n)   n)
       r (rem n2 (count s2))]
  (loop [result (vec s2), counter r]
    (if (zero? counter)
      (if (neg? n)
        (reverse result)
        result)
      (recur (conj (vec (rest result)) (first result)) (dec counter))
    ))))

;; Tests

(println (= (rot-seq 2 [1 2 3 4 5]) '(3 4 5 1 2)))

(println (= (rot-seq -2 [1 2 3 4 5]) '(4 5 1 2 3)))

(println (= (rot-seq 6 [1 2 3 4 5]) '(2 3 4 5 1)))

(println (= (rot-seq 1 '(:a :b :c)) '(:b :c :a)))

(println (= (rot-seq -4 '(:a :b :c)) '(:c :a :b)))