
;; Partially Flatten a Sequence

;; Write a function which flattens any nested combination of sequential things (lists, vectors, etc.), 
;; but maintains the lowest level sequential items. The result should be a sequence of sequences with only one level of nesting.

(defn pf [s]
  (let [nodes (atom [])
        ; linearize the input tree structure by using postwalk
        _ (clojure.walk/postwalk #(if (sequential? %) (swap! nodes conj []) (swap! nodes conj %)) s)
        nodes2 (partition-by sequential? @nodes)
        nodes3 (filter #(not (sequential? (first %))) nodes2)]
    nodes3))

;; Tests

(println (= (pf [["Do"] ["Nothing"]]) [["Do"] ["Nothing"]]))

(println (= (pf [ [[[:a :b]]] [[:c :d]] [:e :f]]) [[:a :b] [:c :d] [:e :f]]))

(println (= (pf '( (1 2) ( (3 4) ((((5 6)))) ) )) '((1 2)(3 4)(5 6))))