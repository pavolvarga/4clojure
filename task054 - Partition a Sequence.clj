
;; Partition a Sequence

;; Write a function which returns a sequence of lists of x items each. Lists of less than x items should not be returned.

(defn own-partition [n, s]
  (let [do-spliting (fn [n2 s2]
                      (loop [result [], s3 s2]
                        (if (<= (count s3) n2)
                          (conj result s3)
                          (let [res (split-at n2 s3)]
                            (recur (conj result (first res)) (second res))))))
        splitted (do-spliting n s)]
    (if (< (count (last splitted)) n)
      (drop-last splitted)
      splitted)))

;; Tests

(println (= (own-partition 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8))))

(println (= (own-partition 2 (range 8)) '((0 1) (2 3) (4 5) (6 7))))

(println (= (own-partition 3 (range 8)) '((0 1 2) (3 4 5))))