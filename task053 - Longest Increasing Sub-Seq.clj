
;; Longest Increasing Sub-Seq

;; Given a vector of integers, find the longest consecutive sub-sequence of increasing numbers. 
;; If two sub-sequences have the same length, use the one that occurs first. 
;; An increasing sub-sequence must have a length of 2 or greater to qualify.

(defn fliss [v]
  (let [get-next (fn  [v i]
                   (let [c (count v)]
                     (loop [result []
                            ii i]
                       (let [ae (v ii)
                             ne (if (= c (inc ii)) nil (v (inc ii)))]
                         (if (and (not= ii c) (not= nil ne) (= ne (inc ae)))
                           (recur (conj result ae) (inc ii))
                           (conj result ae)
                           ))))) 
        find-next (fn  [v i]
                    (let [c (count v)]
                      (cond
                        (= (dec c) i) {:npos c, :subs []}
                        (not= 1 (- (v (inc i)) (v i))) {:npos (inc i), :subs []}
                        :else
                        (let [ss (get-next v i)]
                          {:npos (+ i (count ss)), :subs ss})
                        )))
        get-subsets (fn [v]
                      (loop [i 0, result []]
                        (let [found (find-next v i)]
                          (if (>= (found :npos) (count v))
                            (conj result (found :subs))
                            (recur (found :npos) (conj result (found :subs))))
                          )))
        subsets (get-subsets v)]
    (first (reverse (sort-by count subsets)))
    ))

;; Tests

(println (= (fliss [1 0 1 2 3 0 4 5]) [0 1 2 3]))

(println (= (fliss [5 6 1 3 2 7]) [5 6]))

(println (= (fliss [2 3 3 4 5]) [3 4 5]))

(println (= (fliss [7 6 5 4]) []))