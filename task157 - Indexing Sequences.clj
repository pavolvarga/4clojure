
;; Indexing Sequences

;; Transform a sequence into a sequence of pairs containing the original elements along with their index.

(defn idx-seq [coll] 
  (keep-indexed #(conj [] %2 %1) coll))

;; Tests

(println (= (idx-seq [:a :b :c]) [[:a 0] [:b 1] [:c 2]]))

(println (= (idx-seq [0 1 3]) '((0 0) (1 1) (3 2))))

(println (= (idx-seq [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]]))