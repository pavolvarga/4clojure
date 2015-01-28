
;; Number Maze
 
;; Given a pair of numbers, the start and end point, find a path between the two using only three possible operations:
;;
;;    double
;;    halve (odd numbers cannot be halved)
;;    add 2
;;
;; Find the shortest path through the "maze". Because there are multiple shortest paths, 
;; you must return the length of the shortest path, not the path itself.

(defn fsp [a b]
  (letfn [
    (find-paths [a b]
      (letfn [
        (shrink-path [p]
          "Shrink path by trying to reduce number of +2 operations
           [9 18 20 22 24 12] => [9 11 22 24 12]"
          (letfn [(shrink-for [v s]
                    "starting at `s` position, shrink some path if it is possible"
                    (letfn [(shrunk-if-possible [a b]
                              (when (and (even? b) (not (= (* a 2) (/ b 2))))
                                (loop [x (/ b 2) p [b x]]
                                  (when (<= a x)
                                    (if (= a x)
                                      (reverse p)
                                      (recur (- x 2) (conj p (- x 2))))))))
                            (replace-subvec [new-vec, old-vec, start, end]
                              (let [before (subvec old-vec 0 start)
                                    middle (subvec old-vec start (inc end))
                                    after  (subvec old-vec (inc end))]
                                (concat before new-vec after)))]
                      (loop [i (inc s) result v]
                        (if-not (or (neg? i) (>= i (count v)))
                          (let [shrunk (shrunk-if-possible (v s) (v i))
                                dist (- (inc i) s)]
                            (if (nil? shrunk)
                              (recur (inc i) result)
                              (if (< (count shrunk) dist)
                                (recur -1 (replace-subvec shrunk v s i)) ;found shrunked alternative
                                (recur (inc i) result))))
                          result))))]
            (loop [i 0 result p]
              (if (= i (-> result count dec))
                result
                (let [r (shrink-for result i)]
                  (if (= r result)
                    (recur (inc i) result)
                    ; shrinking was successfull, therefore replace existing path and start from beginning
                    (recur 0 (vec r))))))))
        (find-by-comparing [a b]
          (if (= a b)
            [a]
            nil))
        (find-by-multipling [a b]
          (let [[x y] (reverse (sort [a b]))
                find-path (fn find-path [a b]
                            (loop [x a y b result []] 
                              (if (= x y)
                                (conj result b)
                                (recur (/ x 2) y (conj result x)))))]
            (if-not (zero? (rem x y))
              nil
              (if (> a b)
                (find-path a b)
                (reverse (find-path b a))))))
        (find-by-adding2 [a b]
          (let [find-path (fn find-path [a b]
                            (loop [x a y b result []] 
                              (if (= x y)
                                (conj result b)
                                (recur (+ x 2) y (conj result x)))))]
            (if (or (> a b) (not (zero? (rem (- b a) 2))))
              nil
             (find-path a b))))
        (find-by-combining [a b]
          (loop [x a result [a]]
            (if (= x b)
              result
              (cond
                ;use doubling
                (or (< x b) (and (odd? x) (even? b)))
                (recur (* x 2) (conj result (* x 2)))
                ;use adding2
                (not (zero? (rem x b)))
                (recur (+ x 2) (conj result (+ x 2)))
                ;use halving, but prevent endless loop by constant doubling and havling 
                (zero? (rem x b))
                (let [last-but-2 (- (count result) 3)
                      c (if (neg? last-but-2) nil (result last-but-2))]
                  (if (= x c)
                    (recur (+ x 2) (conj (-> result pop pop) (+ x 2))) ; 2 pops - remove last 2 items
                    (recur (/ x 2) (conj result (/ x 2)))))))))]
        (let [paths (conj []
                      (find-by-comparing a b)
                      (find-by-multipling a b)
                      (find-by-adding2 a b)
                      (shrink-path (find-by-combining a b)))]
          (into [] (filter #(-> % nil? not) paths)))))]
  (count (first (sort-by count (find-paths a b))))))

;; Tests

(println (= 1 (fsp 1 1)))  ; 1

(println (= 3 (fsp 3 12))) ; 3 6 12

(println (= 3 (fsp 12 3))) ; 12 6 3

(println (= 3 (fsp 5 9)))  ; 5 7 9

(println (= 9 (fsp 9 2)))  ; 9 18 20 10 12 6 8 4 2

(println (= 5 (fsp 9 12))) ; 9 11 22 24 12