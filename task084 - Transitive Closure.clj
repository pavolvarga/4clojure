
;; Transitive Closure

;; Write a function which generates the transitive closure of a binary relation. The relation will be represented as a set of 2 item vectors.

(defn gtc [s]
  (let [find-relations  (fn [p s]
                          (filter 
                            (fn [x]
                              (or (= (x 0) (p 1))
                                (= (x 1) (p 0))))
                            s))
        ctc (fn [p1 p2]
              (if (=( p1 0) (p2 1))
                [(p2 0) (p1 1)]
                [(p1 0) (p2 1)]))
        tc  (fn [p s]
              (loop [s1 s result []]
                (if (not-empty s1)
                  (recur 
                    (rest s1) 
                    (conj result (ctc p (first s1)))
                  )
                  result)))
        rtc (fn [s]
              (loop [p (first s) r (rest s) result #{}]
                  (if (not-empty r)
                    (let [rels (find-relations p r)
                          tcr  (tc p rels)]
                      (recur
                        (first r)
                        (rest r)
                        (if (empty? tcr) 
                          (conj result p) 
                          (into (conj result p) tcr)
                        )))
                    (conj result p))))]
    (loop [n 0, result (rtc s)]
      (if (= n (-> s count dec))
        result
        (recur (inc n) (into result (rtc result)))
      ))))

;; Tests

(println (let [divides #{[8 4] [9 3] [4 2] [27 9]}]
  (= (gtc divides) #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]})))

(println (let [more-legs
      #{["cat" "man"] ["man" "snake"] ["spider" "cat"]}]
  (= (gtc more-legs)
     #{["cat" "man"] ["cat" "snake"] ["man" "snake"]
       ["spider" "cat"] ["spider" "man"] ["spider" "snake"]})))  

(println (let [progeny
      #{["father" "son"] ["uncle" "cousin"] ["son" "grandson"]}]
  (= (gtc progeny)
     #{["father" "son"] ["father" "grandson"]
       ["uncle" "cousin"] ["son" "grandson"]})))