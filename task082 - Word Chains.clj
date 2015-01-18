
;; Word Chains

;; A word chain consists of a set of words ordered so that each word differs by only one letter from the words directly before and after it. 
;; The one letter difference can be either an insertion, a deletion, or a substitution. Here is an example word chain:
;;
;; cat -> cot -> coat -> oat -> hat -> hot -> hog -> dog
;;
;; Write a function which takes a sequence of words, and returns true if they can be arranged into one continous word chain, and false if they cannot.

;; Not accepted because of timeout
(defn is-word-chain? [s]
	(let [	gen-permutations (fn gen-permutations [s]
								(let [	new-comb (fn [e i s]
													(let [s1 (vec s)]
														(conj (assoc s1 i e) (s1 i))))
										combine (fn [e s]
													(if (not (sequential? s))
														[[e s] [s e]]
														(let [n (count s)]
															(loop [i 0, result []]
																(if (= i n)
																	(conj result (conj s e))
																	(recur (inc i) (conj result (new-comb e i s)))
															)))))]
								(if (= 1 (count s))
									s
									(let [f 	(first s)
										  r 	(rest s)
										  perms (gen-permutations r)]
										(loop [result [], perms1 perms]
											(if (empty? perms1)
												result
												(let [coms (combine f (first perms1))]
												(recur (into result coms) (rest perms1))
											)))))))
			are-words-connected? (fn [word1 word2]
									(let [	is-substitution? (fn [word1 word2]
																(loop [w1 (seq word1) 
																	   w2 (seq word2)
																	   diff-chars 0]
																	(if (empty? w1)
																		(= 1 diff-chars)
																		(let [c1 (first w1)
																			  c2 (first w2)]
																			  (recur
																			  	(rest w1)
																			  	(rest w2)
																			  	(if (not= c1 c2) (inc diff-chars) diff-chars))
																			))))
											is-deletion-or-insertion? (fn [word1 word2]
																		(let [w1 (vec word1)
																			  w2 (vec word2)
																			  n1 (count w1)
																			  n2 (count w2)
																			  longer  (if (> n1 n2) w1 w2)
																			  shorter (if (> n1 n2) w2 w1)
																			  proc-chars (fn [shorter longer]
																							(loop [shor shorter, result []]
																								(if (empty? shor)
																									result
																									(let [c (first shor)]
																										(recur (rest shor) (conj result (.indexOf longer c)))
																										))))
																			  are-chars-ok? (fn [idxs]
																								(if (some neg? idxs)
																									false
																									(loop [idx1 idxs, result []]
																										(if (empty? idx1)
																											(let [trouble (count (filter #(or (neg? %) (> % 1)) result))]
																												(or (= 0 trouble) (= 1 trouble)))
																											(let [i (first idx1)
																												  ni (if (= 1 (count idx1)) nil (-> idx1 rest first))]
																												(recur
																													(rest idx1)
																													(if (nil? ni) result (conj result (- ni i)))
																												))))))]
																					(are-chars-ok? (proc-chars shorter longer))
																			  ))
											n1  (count word1)
											n2  (count word2)
											n   (Math/abs (- n1 n2))]
											(cond
												(= 0 n) (is-substitution? word1 word2)
												(= 1 n) (is-deletion-or-insertion? word1 word2)
												:default false)
									))
			prepare-seq		(fn [v]
								(let [sv (subvec v 1 (dec (count v)))]
									(loop [sv1 sv, result [(first v)]]
										(if (empty? sv1)
											(partition 2 (conj result (last v)))
											(recur 
												(rest sv1) 
												(conj result (first sv1) (first sv1))
											)
										))))	
			permutations 	(gen-permutations s)
			is-connected?	(fn [s]
								(loop [s1 s, result #{}]
									(if (empty? s1)
										(and (= 1 (count result)) (true? (first result)))
											(let [w1 (-> s1 first first)
												  w2 (-> s1 first last)
												  connected (are-words-connected? w1 w2)]
												(recur (rest s1) (conj result connected))
											))))
			connected (some #(-> % prepare-seq is-connected?) permutations)] 
		(true? connected)
		))

;; Accepted solution
(defn is-word-chain2? [s]
	(let [  next-perm (fn [s]
						(let [v (vec s)
							find-pivot (fn [v]
											(let [n (-> v count dec)]
												(loop [i (dec n), pivot nil]
													(if (or (neg? i) (not= pivot nil))
														{:pivot pivot :idx (if (nil? pivot) i (inc i))}
														(recur
															(dec i)
															(let [ei (v i)
																  ri (v (inc i))]
																  (if (neg? (compare ei ri)) ei nil)))
												))))
							find-successor	(fn [v pivot]
												(let [n (-> v count dec)]
													(loop [i n]
														(let [e (v i)]
															(if (pos? (compare e (pivot :pivot)))
																{:successor e, :idx i}
																(recur (dec i))
															)))))
							swap			(fn [v i1 i2]
												(let [e1 (v i1)
													  e2 (v i2)]
													  (assoc (assoc v i1 e2) i2 e1)))
							reverse-from 	(fn [v start]
												(let [l (subvec v 0 start)
													  r (subvec v start)
													  rev-r (reverse r)]
													  (flatten (conj l rev-r))
													))
							pivot   (find-pivot v)]
							(if (nil? (pivot :pivot))
								(reverse v)
								(let [successor (find-successor v pivot)
								      p-idx (pivot :idx)
								      s-idx (successor :idx)  
									  swaped (swap v p-idx s-idx)]
									  (reverse-from swaped (inc p-idx))
								))))	
			are-words-connected? (fn [word1 word2]
									(let [	is-substitution? (fn [word1 word2]
																(loop [w1 (seq word1) 
																	   w2 (seq word2)
																	   diff-chars 0]
																	(if (empty? w1)
																		(= 1 diff-chars)
																		(let [c1 (first w1)
																			  c2 (first w2)]
																			  (recur
																			  	(rest w1)
																			  	(rest w2)
																			  	(if (not= c1 c2) (inc diff-chars) diff-chars))
																			))))
											is-deletion-or-insertion? (fn [word1 word2]
																		(let [w1 (vec word1)
																			  w2 (vec word2)
																			  n1 (count w1)
																			  n2 (count w2)
																			  longer  (if (> n1 n2) w1 w2)
																			  shorter (if (> n1 n2) w2 w1)
																			  proc-chars (fn [shorter longer]
																							(loop [shor shorter, result []]
																								(if (empty? shor)
																									result
																									(let [c (first shor)]
																										(recur (rest shor) (conj result (.indexOf longer c)))
																										))))
																			  are-chars-ok? (fn [idxs]
																								(if (some neg? idxs)
																									false
																									(loop [idx1 idxs, result []]
																										(if (empty? idx1)
																											(let [trouble (count (filter #(or (neg? %) (> % 1)) result))]
																												(or (= 0 trouble) (= 1 trouble)))
																											(let [i (first idx1)
																												  ni (if (= 1 (count idx1)) nil (-> idx1 rest first))]
																												(recur
																													(rest idx1)
																													(if (nil? ni) result (conj result (- ni i)))
																												))))))]
																					(are-chars-ok? (proc-chars shorter longer))
																			  ))
											n1  (count word1)
											n2  (count word2)
											n   (Math/abs (- n1 n2))]
											(cond
												(= 0 n) (is-substitution? word1 word2)
												(= 1 n) (is-deletion-or-insertion? word1 word2)
												:default false)
									))
			prepare-seq		(fn [v]
								(let [sv (subvec v 1 (dec (count v)))]
									(loop [sv1 sv, result [(first v)]]
										(if (empty? sv1)
											(partition 2 (conj result (last v)))
											(recur 
												(rest sv1) 
												(conj result (first sv1) (first sv1))
											)
										))))	
			is-connected?	(fn [s]
								(loop [s1 s, result #{}]
									(if (empty? s1)
										(and (= 1 (count result)) (true? (first result)))
											(let [w1 (-> s1 first first)
												  w2 (-> s1 first last)
												  connected (are-words-connected? w1 w2)]
												(recur (rest s1) (conj result connected))
											))))
			v (vec (sort s))]
		(loop [v1 (next-perm v)]
			(if (= v1 v)
				false
				(if (is-connected? (prepare-seq (vec v1)))
					true
					(recur (next-perm v1) ))))
		))


;; Tests

(println (= true  (is-word-chain2? #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"})))

(println (= false (is-word-chain2? #{"cot" "hot" "bat" "fat"})))

(println (= false (is-word-chain2? #{"to" "top" "stop" "tops" "toss"})))

(println (= true  (is-word-chain2? #{"spout" "do" "pot" "pout" "spot" "dot"})))

(println (= true  (is-word-chain2? #{"share" "hares" "shares" "hare" "are"})))

(println (= false (is-word-chain2? #{"share" "hares" "hare" "are"})))