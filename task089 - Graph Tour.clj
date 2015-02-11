
;; Graph Tour
;;
;; Starting with a graph you must write a function that returns true if it is possible
;; to make a tour of the graph in which every edge is visited exactly once.
;;
;; The graph is represented by a vector of tuples, where each tuple represents a single edge.
;;
;; The rules are:
;; 
;; - You can start at any node.
;; - You must visit each edge exactly once.
;; - All edges are undirected.

;; http://en.wikipedia.org/wiki/Eulerian_path
(defn egt? [graph]
  (let [is-connected? (fn [edges]
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
        calc-vertex-degree  (fn [grap]
                              (loop [g grap result {}]
                                (if (empty? g)
                                  result
                                  (let [[v1 v2] (first g)
                                        s1 (update-in result [v1] #(if (nil? %) 1 (inc %)))
                                        s2 (update-in s1     [v2] #(if (nil? %) 1 (inc %)))]
                                    (recur (rest g) s2)))))
        calced-graph (calc-vertex-degree graph) 
        ; count of vertexes with odd degree
        odd-vertex (count (filter odd? (vals calced-graph)))
        ; for the existence of Eulerian trails it is necessary that zero or two vertices have an odd degree
        is-eulerian-trail-pos? (fn [odd-vertex] (or (zero? odd-vertex) (= 2 odd-vertex)))
        connected (is-connected? graph)
        trail-pos (is-eulerian-trail-pos? odd-vertex)]
  (if (or (not connected) (not trail-pos))
    false
    true
)))

;; Tests

(println (= true (egt? [[:a :b]])))

(println (= false (egt? [[:a :a] [:b :b]])))

(println (= false (egt? [[:a :b] [:a :b] [:a :c] [:c :a] [:a :d] [:b :d] [:c :d]])))

(println (= true (egt? [[1 2] [2 3] [3 4] [4 1]])))

(println (= true (egt? [[:a :b] [:a :c] [:c :b] [:a :e] [:b :e] [:a :d] [:b :d] [:c :e] [:d :e] [:c :f] [:d :f]])))

(println (= false (egt? [[1 2] [2 3] [2 4] [2 5]])))