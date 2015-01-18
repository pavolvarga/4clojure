
;; Lazy Searching

;; Given any number of sequences, each sorted from smallest to largest, find the smallest single number which appears in all of the sequences. 
;; The sequences may be infinite, so be careful to search lazily.

(defn lazy-search [& sequences]
  (let [same? (fn [s]
                (if (= 1 (count (distinct s)))
                  true
                  false)) 
        find-smallest (fn [s]
                        (loop [s1 s, smallest nil]
                          (if (empty? s1)
                            smallest
                            (let [new-smallest (cond
                                                 (nil? smallest) (first s1)
                                                 (< (first s1) smallest) (first s1)
                                                 :else smallest)]
                              (recur (rest s1) new-smallest)))))
        mark-interest (fn [s]
                        (let [smallest (find-smallest s)]
                          (loop [s1 s, result []]
                            (if (empty? s1)
                              result
                              (let [value (if (= smallest (first s1)) true false)]
                                (recur (rest s1) (conj result value))
                                )))))
        ext-values (fn [seqs, stashed, marks]
                     (loop [seqs2 seqs, result [], marks2 marks, stashed2 stashed]
                       (if (empty? seqs2)
                         result
                         (let [s (first seqs2), mark (first marks2)]
                           (recur
                             (rest seqs2)
                             (if (true? mark) 
                               (conj result (first s)) 
                               (conj result (first stashed2)))
                             (rest marks2)
                             (rest stashed2))
                           ))))
        ext-seqs (fn ext-seqs [seqs, marks]
                   (loop [seqs2 seqs, result [], marks2 marks]
                     (if (empty? seqs2)
                       result
                       (let [s (first seqs2), mark (first marks2)]
                         (recur
                           (rest seqs2)
                           (if (true? mark)
                             (conj result (drop 1 s))
                             (conj result s))
                           (rest marks2)
                           )))))]
  (loop [stashed (repeat (count sequences) 0), seqs sequences]
    (let [marks (mark-interest stashed)
          values (ext-values seqs, stashed, marks)
          ss (ext-seqs seqs, marks)]
      (if (same? values)
        (first values)
        (recur values ss))
      ))))

;; Tests

(println (= 3  (lazy-search [3 4 5])))

(println (= 4  (lazy-search [1 2 3 4 5 6 7] [0.5 3/2 4 19])))

(println (= 7  (lazy-search (range) (range 0 100 7/6) [2 3 5 7 11 13])))

(println (= 64 (lazy-search (map #(* % % %) (range)) ;; perfect cubes
                            (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
                            (iterate inc 20))))