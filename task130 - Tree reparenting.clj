
;; Tree reparenting
;;
;; Every node of a tree is connected to each of its children as well as its parent. 
;; One can imagine grabbing one node of a tree and dragging it up to the root position, 
;; leaving all connections intact. 
;; For example, on image `task130 - Tree reparenting.png` on the left is a binary tree. 
;; By pulling the "c" node up to the root, we obtain the tree on the right. 
;;
;; Note it is no longer binary as "c" had three connections total -- two children and one parent. 
;; Each node is represented as a vector, which always has at least one element giving the name of the node as a symbol. 
;; Subsequent items in the vector represent the children of the node. 
;; Because the children are ordered it's important that the tree you return keeps the children of each node 
;; in order and that the old parent node, if any, is appended on the right. 
;; Your function will be given two args -- the name of the node that should become the new root, and the tree to transform.

;; Not accepted because `require` is not allowed
(require '[clojure.zip :as zip])
(defn reparent-tree1 [new-root tree]
  (letfn [
    ; create zipper for tree representation used in this task
    (make-zipper [tree]
      (letfn [
        (branch?  [node] (> (count node) 1))
        (children [node] (when (branch? node) (rest node)))
        (make-node [node childs]
          (if (nil? childs)
            (conj '() (first node))
            (if (-> childs first seq?)
              (conj childs (first node))
              (seq (conj (vec node) childs)))))]
        (zip/zipper branch? children make-node tree)))
    ; find loc of `node` starting at `loc-input`
    (find-loc [node loc-input]
      (loop [loc loc-input]
        (if (zip/end? loc)
          nil
          (if (or (= node (zip/node loc)) (= node (first (zip/node loc))))
            loc
            (recur (zip/next loc))))))
    ; create path from loc to root (bottom up)
    (make-path [loc]
      (loop [result [] actual loc]
        (if (nil? actual)
          result
          (recur (conj result (-> actual zip/node first)) (zip/up actual)))))
    ; make a current root the righmost childred of the `new-root`
    (reparent-one-level [tree new-root last-move] 
      (let [target-loc (find-loc new-root (make-zipper tree))
            ; subtree starting at `new-root`
            nr-subtree (zip/node target-loc)
            ; tree without `nr-subtree` subtree
            tree-without-nr (-> target-loc zip/remove zip/root)
            ; to be left sibling
            sibling (-> (make-zipper nr-subtree) zip/down zip/rightmost)]
        (if last-move
          (zip/make-node target-loc nr-subtree tree-without-nr)
          (zip/root (zip/insert-right sibling tree-without-nr)))))]
    (let [target-loc (find-loc new-root (make-zipper tree))
          ; path of neccessary changes from up to bottom (starting at root and ending an new-root)
          path (-> target-loc make-path reverse)
          ; how many times the tree must changes
          moves (dec (count path))]
      (loop [i moves p (rest path) t tree]
        (if (zero? i)
          t
          (recur (dec i) (rest p) (reparent-one-level t (first p) (= 1 i)))))
)))

;; Accepted solution
(defn reparent-tree2 [new-root tree]
  (letfn [
    ; true if the node is a branch
    (is-branch? [node] (> (count node) 1))
    ; get node's name
    (gnn [node] (first node))
    ; get node's children
    (gnkids [node] (if (= 1 (count node)) '() (rest node)))
    ; find path from root to target
    (find-path [target tree]
      (let [path (atom [])
            traverse (fn traverse [node]
                        (let [nname (gnn node)]
                          (if (and (empty? @path) (= target nname))
                            (swap! path conj nname)
                            (doseq [kid (gnkids node)] 
                              (when (empty? @path) 
                                (traverse kid)
                                (when (-> @path empty? not)
                                  (swap! path conj nname)))))))]
        (traverse tree)
        (reverse @path)))
    ; delete a subtree whose name is equal to target
    (delete-subtree [target tree] 
      (letfn [(traverse [node]
                (let [nname (gnn node)
                      nkids (gnkids node)]
                  (when-not (= target nname)
                    (if (empty? nkids)
                      node
                      (cons nname (filter #(not= nil %) (map traverse nkids)))))))]
        (traverse tree)))
    ; extract a subtree whose name is equal to target
    (extract-subtree [target tree]
      (let  [result (atom nil)
             traverse (fn traverse [node]
                        (if (= target (gnn node))
                          (reset! result node)
                          (doseq [kid (gnkids node)]
                            (traverse kid))))]
        (traverse tree)
        @result))
    ; insert as rightmost child
    (insert-at-rightmost [node child]
      (let [nname (gnn node)
            nkids (gnkids node)
            new-kids  (if (empty? nkids) 
                        child 
                        (list* (conj (vec nkids) child)))
            new-node  (if (-> new-kids first seq?) 
                        (conj new-kids nname) 
                        (reverse (conj '() nname new-kids)))]
        new-node))
    ; make a current root the righmost childred of the `new-root`
    (reparent-one-level [tree new-root]
      (let [nr-subtree (extract-subtree new-root tree)
            tree-without-nr (delete-subtree new-root tree)]
        (insert-at-rightmost nr-subtree tree-without-nr)))]
    (let [; path of neccessary changes from up to bottom (starting at root and ending an new-root)
          path (find-path new-root tree)
          ; how many times the tree must changes
           moves (dec (count path))]
      (loop [i moves p (rest path) t tree]
        (if (zero? i)
          t
          (recur (dec i) (rest p) (reparent-one-level t (first p))))))))

;; Tests

(println (= '(n)
   (reparent-tree2 'n '(n) )))

(println (= '(a (t (e)))
   (reparent-tree2 'a '(t (e) (a)) )))

(println (= '(e (t (a)))
   (reparent-tree2 'e '(a (t (e))) )))

(println (= '(a (b (c)))
   (reparent-tree2 'a '(c (b (a))) )))

(println (= '(d 
                (b
                  (c)
                  (e)
                  (a 
                    (f 
                      (g) 
                      (h)))))
  (reparent-tree2 'd '(a
                        (b 
                          (c) 
                          (d) 
                          (e))
                        (f 
                          (g)
                          (h))) )))

(println (= '(c 
                (d) 
                (e) 
                (b
                  (f 
                    (g) 
                    (h))
                  (a
                    (i
                      (j
                        (k)
                        (l))
                      (m
                        (n)
                        (o))))))
   (reparent-tree2 'c '(a
                        (b
                          (c
                            (d)
                            (e))
                          (f
                            (g)
                            (h)))
                        (i
                          (j
                            (k)
                            (l))
                          (m
                            (n)
                            (o)))) )))