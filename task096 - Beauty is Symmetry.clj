
;; Beauty is Symmetry

;; Let us define a binary tree as "symmetric" if the left half of the tree is the mirror image of the right half of the tree. 
;; Write a predicate to determine whether or not a given binary tree is symmetric. 
;; (see To Tree, or not to Tree for a reminder on the tree representation we're using)

(defn sym-bin-tree? [v]
  (let [[r lt rt] v
       com-bin-tree? (fn com-bin-tree? [lt rt]
           (if-not (and (= 3 (count lt)) (= 3 (count rt)))
		    false
			  (let [
			        [lt-r lt-lc lt-rc] lt
			        [rt-r rt-lc rt-rc] rt
			        r-same  (= lt-r  rt-r)
		         	lt-lc-seq (sequential? lt-lc)
		            rt-lc-seq (sequential? rt-lc)
		            lt-rc-seq (sequential? lt-rc)
		            rt-rc-seq (sequential? rt-rc)]
			    (cond
		       		(not r-same) false
		         	(and (not lt-lc-seq) rt-rc-seq) false
		            (and (not lt-rc-seq) rt-lc-seq) false
		     		(and (not rt-lc-seq) lt-rc-seq) false
		       		(and (not rt-rc-seq) lt-lc-seq) false
		         	(and (not lt-lc-seq) (not rt-rc-seq) (not= lt-lc rt-rc)) false
		          	(and (not lt-rc-seq) (not rt-lc-seq) (not= lt-rc rt-lc)) false
		         	(and lt-lc-seq rt-rc-seq) (com-bin-tree? lt-lc rt-rc)
		            (and lt-rc-seq rt-lc-seq) (com-bin-tree? lt-rc rt-lc)
		            (and (= lt-lc rt-rc) (= lt-rc rt-lc)) true
		         ))))]
  (com-bin-tree? lt rt)))

;; Tests

(println (= (sym-bin-tree? '(:a (:b nil nil) (:b nil nil))) true))

(println (= (sym-bin-tree? '(:a (:b nil nil) nil)) false))

(println (= (sym-bin-tree? '(:a (:b nil nil) (:c nil nil))) false))

(println (= (sym-bin-tree? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
   true))

(println (= (sym-bin-tree? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
   false))

(println (= (sym-bin-tree? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [6 nil nil] nil]] nil]])
   false))