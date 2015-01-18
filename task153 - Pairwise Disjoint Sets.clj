
;; Pairwise Disjoint Sets

;; Given a set of sets, create a function which returns true if no two of those sets have any elements in common1 and false otherwise. 
;; Some of the test cases are a bit tricky, so pay a little more attention to them.
;;
;; Such sets are usually called pairwise disjoint or mutually disjoint.

(defn par-disj-sets? [sets]
  (let [
  		combine (fn [v]
	             (let [rem-el (fn [e i] (remove #(= e %) i))]
				    (loop [result [] v2 v]
				      (let [  hd (first v2)
				              tl (rest v2)
				              other (rem-el hd tl)
				              combs (for [x other] [hd x])]
				      (if (empty? v2)
				        result
				        (recur (into result combs) tl)
				      ))
				    )))
        is-in? (fn [s1 s2] 
		          (let [r (clojure.set/intersection s1 s2)]
						    (if (empty? r)
						      false
						      true)))
        combinations (combine sets)
        result (set (for [x combinations] (is-in? (x 0) (x 1))))]
    (not (contains? result true))))

;; Tests

(println (= (par-disj-sets? #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
   true))

(println (= (par-disj-sets? #{#{:a :b :c :d :e}
         #{:a :b :c :d}
         #{:a :b :c}
         #{:a :b}
         #{:a}})
   false))

(println (= (par-disj-sets? #{#{[1 2 3] [4 5]}
         #{[1 2] [3 4 5]}
         #{[1] [2] 3 4 5}
         #{1 2 [3 4] [5]}})
   true))

(println (= (par-disj-sets? #{#{'a 'b}
         #{'c 'd 'e}
         #{'f 'g 'h 'i}
         #{''a ''c ''f}})
   true))

(println (= (par-disj-sets? #{#{'(:x :y :z) '(:x :y) '(:z) '()}
         #{#{:x :y :z} #{:x :y} #{:z} #{}}
         #{'[:x :y :z] [:x :y] [:z] [] {}}})
   false))

(println (= (par-disj-sets? #{#{(= "true") false}
         #{:yes :no}
         #{(class 1) 0}
         #{(symbol "true") 'false}
         #{(keyword "yes") ::no}
         #{(class '1) (int \0)}})
   false))

(println (= (par-disj-sets? #{#{distinct?}
         #{#(-> %) #(-> %)}
         #{#(-> %) #(-> %) #(-> %)}
         #{#(-> %) #(-> %) #(-> %)}})
   true))

(println (= (par-disj-sets? #{#{(#(-> *)) + (quote mapcat) #_ nil}
         #{'+ '* mapcat (comment mapcat)}
         #{(do) set contains? nil?}
         #{, , , #_, , empty?}})
   false))