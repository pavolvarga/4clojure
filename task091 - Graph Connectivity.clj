
;; Graph Connectivity
 
;; Given a graph, determine whether the graph is connected. A connected graph is such that a path exists between any two given nodes.
;;    -Your function must return true if the graph is connected and false otherwise.
;;    -You will be given a set of tuples representing the edges of a graph. Each member of a tuple being a vertex/node in the graph.
;;    -Each edge is undirected (can be traversed either direction).

(defn is-connected? [edges]
  (if (empty? (rest edges))
    true
    (let [vertexes (-> edges vec flatten set)
          n        (count vertexes)
          get-reachable-from  (fn [x edges]
                                (loop [eds edges, result #{}]
                                  (if (empty? eds)
                                    result
                                    (let [edge (first eds)
                                          idx (.indexOf edge x)]
                                      (if (not= -1 idx)
                                        (recur 
                                          (rest eds) 
                                          (conj result (if (= 0 idx) (edge 1) (edge 0))))
                                        (recur 
                                          (rest eds) 
                                          result))))))
          find-edges-with (fn [x edges]
                            (loop [eds edges, result #{}]
                              (if (empty? eds)
                                result
                                (let [edge (first eds)
                                      idx  (.indexOf edge x)]
                                  (if (= -1 idx)
                                    (recur (rest eds) result)
                                    (recur (rest eds) (conj result edge)))))))
          get-vertexes-without  (fn [y edges]
                                  (loop [eds edges, result #{}]
                                    (if (empty? eds)
                                      result
                                      (let [[a b] (first eds)]
                                        (cond
                                          (= a y) (recur (rest eds) (conj result b))
                                          (= b y) (recur (rest eds) (conj result a))
                                          :default
                                          (recur (rest eds) result))))))
          fill-l-k  (fn [l k vers-without-y]
                      (loop [vwy vers-without-y, l1 (set l), k1 k]
                        (if (empty? vwy)
                          {:l l1, :k k1}
                          (let [z (first vwy)]
                            (if (contains? l1 z)
                              (recur (rest vwy) l1 k1)
                              (recur (rest vwy) (conj l1 z) (conj k1 z)))))))
          ver-x (first vertexes)
          l [ver-x]
          k [ver-x]]
      (loop [k1 k, l1 k1]
        (if (empty? k1)
          (if (= n (count l1)) true false)
          (let [y (first k1)
                edgs-y (find-edges-with y edges)
                vers-without-y (get-vertexes-without y edgs-y)
                r (fill-l-k l1 (rest k1) vers-without-y)]
            (recur (r :k) (r :l))))))))

;; Tests

(println (= true  (is-connected? #{[:a :a]})))

(println (= true  (is-connected? #{[:a :b]})))

(println (= false (is-connected? #{[1 2] [2 3] [3 1] [4 5] [5 6] [6 4]})))

(println (= true  (is-connected? #{[1 2] [2 3] [3 1] [4 5] [5 6] [6 4] [3 4]})))

(println (= false (is-connected? #{[:a :b] [:b :c] [:c :d] [:x :y] [:d :a] [:b :e]})))

(println (= true  (is-connected? #{[:a :b] [:b :c] [:c :d] [:x :y] [:d :a] [:b :e] [:x :a]})))