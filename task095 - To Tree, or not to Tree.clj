
;; To Tree, or not to Tree

;; Write a predicate which checks whether or not a given sequence represents a binary tree. 
;; Each node in the tree must have a value, a left child, and a right child.

(defn binary-tree? [s]
  (if-not (= 3 (count s))
    false
    (let [[a b c] (vec s)]
      (cond
        (sequential? a) (binary-tree? a)
        (sequential? b) (binary-tree? b)
        (sequential? c) (binary-tree? c)
        (false? a) false
        (false? b) false
        (false? c) false
        :else true))))

;; Tests

(println (= (binary-tree? '(:a (:b nil nil) nil)) true))

(println (= (binary-tree? '(:a (:b nil nil))) false))

(println (= (binary-tree? [1 nil [2 [3 nil nil] [4 nil nil]]]) true))

(println (= (binary-tree? [1 [2 nil nil] [3 nil nil] [4 nil nil]]) false))

(println (= (binary-tree? [1 [2 [3 [4 nil nil] nil] nil] nil]) true))

(println (= (binary-tree? [1 [2 [3 [4 false nil] nil] nil] nil]) false))

(println (= (binary-tree? '(:a nil ())) false))