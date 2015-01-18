
;; Equivalence Classes

;; A function f defined on a domain D induces an equivalence relation on D, as follows: 
;; a is equivalent to b with respect to f if and only if (f a) is equal to (f b). 
;; Write a function with arguments f and D that computes the equivalence classes of D with respect to f.

(defn comp-eq-cases [fun d]
  (let [get-values (fn [ci c] (for [i ci] ((vec c) i) ))
        vd (-> d vec sort)
        vs (map fun vd)
        idxs (take (count vd) (range))
        vsi (map vector vs idxs)
        vsi2 (sort-by #(first %) vsi)
        vsi3 (partition-by #(first %) vsi2)
        idx2 (map (fn [s] (map #(second %) s)) vsi3)
        r (for [s idx2] (set (get-values s vd)))]
    (set r)))

;; Tests

(println (= (comp-eq-cases #(* % %) #{-2 -1 0 1 2}) #{#{0} #{1 -1} #{2 -2}}))

(println (= (comp-eq-cases #(rem % 3) #{0 1 2 3 4 5 }) #{#{0 3} #{1 4} #{2 5}}))

(println (= (comp-eq-cases identity #{0 1 2 3 4}) #{#{0} #{1} #{2} #{3} #{4}}))

(println (= (comp-eq-cases (constantly true) #{0 1 2 3 4}) #{#{0 1 2 3 4}}))