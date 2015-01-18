
;; Triangle Minimal Path

;; Write a function which calculates the sum of the minimal path through a triangle. 
;; The triangle is represented as a collection of vectors. 
;; The path should start at the top of the triangle and move to an adjacent number on the next row until the bottom of the triangle is reached.

;; Calculates the Sum of the Minimal Path Through a Triangle
(defn csmptt [t]
  (let [; convert decimal number `d` to a binary represented as vector of binary digits
        dec-to-bin (fn [d]
                     (let [s (Integer/toBinaryString d)]
                       (loop [s1 s, result []]
                         (if (empty? s1)
                           result
                           (recur 
                             (rest s1) 
                             (conj result (-> s1 first str Integer/parseInt))))
                         )))
        ; append leading zeros to a binary number in `v`, so that number of digits is equal to `expz`
        app-led-zeros (fn [v expz]
                        (let [c (count v)]
                          (if (= c expz)
                            v
                            (let [m (vec (repeat (- expz c) 0))]
                              (into m v)
                              ))))
        ; get values from triangle `t` for path `p` (path is represented as a vector of digits for a binary number)
        ; length of `p` is number of levels of `t` - 1
        ; the triangle `t` must be represented as a vector
        vfp (fn [t p]
              (loop [p1 p
                     result [((first t) 0)]						   ; take the root
                     l 1             							   ; skip the root (zero) level
                     prev-idx 0]							       ; index of a value for previous level
                (if (empty? p1)
                  result
                  (let [lp (first p1) 							   ; left (0) or right (1)
                        lev (t l)       						   ; vector at level
                        v (if (zero? lp) (lev prev-idx) (lev (inc prev-idx)))] 	    					   ; value at position
                    (recur
                      (rest p1)
                      (conj result v)
                      (inc l)
                      (if (zero? lp) prev-idx (inc prev-idx)))     ; for moving right - increase index, for left - use same index
                    )           
                  )))
        triangle (vec t)										   ; use rather vector than a list - vfp counts on it
        levels (count triangle)
        num-of-paths (int (Math/pow 2 (dec levels)))
        paths1 (map #(dec-to-bin %) (range num-of-paths))
        paths2 (map #(app-led-zeros % (dec levels)) paths1)
      	vals-at-paths (map #(vfp triangle %) paths2)  
        sums-at-paths (map #(reduce + %) vals-at-paths)
        ]
    (-> sums-at-paths sort first)
    ))

;; Tests

(println (= 7 (csmptt '([1]
          [2 4]
         [5 1 4]
        [2 3 4 5])))) ; 1->2->1->3

(println (= 20 (csmptt '([3]
           [2 4]
          [1 9 3]
         [9 9 2 4]
        [4 6 6 7 8]
       [5 7 3 5 1 4])))) ; 3->4->3->2->7->1