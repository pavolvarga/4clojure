
;; Language of a DFA

;; a deterministic finite automaton (dfa) is an abstract machine that recognizes a regular language. 
;; usually a dfa is defined by a 5-tuple, but instead we'll use a map with 5 keys:
;;
;;  :states is the set of states for the dfa.
;;  :alphabet is the set of symbols included in the language recognized by the dfa.
;;  :start is the start state of the dfa.
;;  :accepts is the set of accept states in the dfa.
;;  :transitions is the transition function for the dfa, mapping :states ⨯ :alphabet onto :states.
;;
;; write a function that takes as input a dfa definition (as described above) and returns a 
;; sequence enumerating all strings in the language recognized by the dfa. 
;; note: although the dfa itself is finite and only recognizes finite-length strings it can still 
;; recognize an infinite set of finite-length strings. and because stack space is finite, 
;; make sure you don't get stuck in an infinite loop that's not producing results every so often!

; WORKS in clojure 1.7.0 only for the first and second test
; returns full collection (not a lazy collection)
; expects transition from state to state to be only for a single character
; will get stuck for tests 4, 5, 6
(defn gen-strings1 [dfa]
  (letfn [
    ; create state transition table from the deterministic finite automaton `dfa`
    (cstt [dfa]
      (letfn [
        ; is the state final
        (is-final? [state] (contains? (dfa :accepts) state))
        ; create state transition table row
        (csttr [dfa current]
          (letfn [
            (collect-states [dfa current]
              (let [trans ((dfa :transitions) current)]
                (loop [alps (keys trans) row []]
                  (if (empty? alps)
                    row
                    (let [k (first alps)]
                      (recur (rest alps) (conj row [(trans k) k])))))))
            (merge-values [row]
              (loop [r row result {}]
                (if (empty? r)
                  result
                  (let [[state event] (first r)]
                    (recur 
                      (rest r)
                      (update-in result [state] conj event))))))
            (process-merged-row [row]
              (if (empty? row)
                row
                (update row (-> row keys first) reverse)))
            (create-row [merged-row current]
              (if (empty? merged-row)
                [{:cs current
                 :final (is-final? current)}]
                (loop [mr merged-row sttr []]
                  (if (empty? mr)
                    sttr
                    (recur 
                      (rest mr) 
                      (conj sttr {:ns (key (first mr))
                                  :chars (val (first mr))
                                  :cs current
                                  :final (is-final? current)}))))))]
            (create-row (process-merged-row (merge-values (collect-states dfa current))) current)))]
      (loop [states (sort (dfa :states)) result []]
        (if (empty? states)
          result
          (recur (rest states) (conj result (csttr dfa (first states))))))))
    ; find transition to the state `state` where the states status `state-status` is either :cs or :ns
    (find-trans [stt state state-status]
      (loop [stt1 stt]
        (when-not (empty? stt1)
          (let [st (-> stt1 first first)]
            (if (= (st state-status) state)
              (first stt1)
              (recur (rest stt1)))))))
    ; find transitions to current state
    (fttcs [stt state] (find-trans stt state :cs))
    ; find transitions to next state
    (fttns [stt state] (find-trans stt state :ns))
    ; return true if the `path` contains path `p`
    (contains-path? [p paths] (not (empty? (filter #(= % p) paths))))
    ; remove paths from transitions
    ; removing alrady explored paths from transitions enables finding different / new paths
    (rpft [transitions paths]
      (letfn [
        (remove-trans [transitions trans] (remove #(= trans %) transitions))
        ; remove single path from transitions
        ; from the `transitions` remove the single path `p`
        (rspft [transitions p]
          (if-not (> (count transitions) 1)
            transitions
            (loop [transitions1 transitions p1 p]
              (if (empty? p1)
                transitions1
                (recur (remove-trans transitions1 (first p1)) (rest p1))))))]
      (loop [transitions1 transitions paths1 paths]
        (if (empty? paths1)
          transitions1
          (recur (rspft transitions1 (first paths1)) (rest paths1))))))
    ; find path in the `stt` to the state `state`
    ; the `current-path` must be an atom into which transitions leading to a final state (including) are stored
    (find-path [stt state current-path paths]
      ; (println "----------------")
      ; (println "state: " state)
      ; (println "current-path: " @current-path)
      ; (println "paths: " paths)
      ; (println "transitions: " (fttcs stt state))
      (loop [transitions (rpft (fttcs stt state) paths)]
        (if (empty? transitions)
          @current-path
          (let [trans (first transitions)
                _ (swap! current-path conj trans)
                cont-path (contains-path? @current-path paths)
                final-state (-> trans :final)]
            (cond 
              ; not a final state - continue
              (not final-state)                       (find-path stt (trans :ns) current-path paths)
              ; it is a final state and is not in the collection of already found states
              (and final-state (not cont-path))       (recur (rest transitions))
              ;it is an already found final state and it is possible to go further - so go further
              (and final-state cont-path (trans :ns)) (find-path stt (trans :ns) current-path paths)
              ;found everything we could - stop
              :default                                (recur (rest transitions)))))))
    ; find all paths leading to all final states in the state transition table `stt`
    ; starting from the starting state `start-state`
    (find-paths [stt start-state]
      (loop [paths []]
        (let [p (find-path stt start-state (atom []) paths)]
          (if (or (empty? p) (contains-path? p paths))
            paths
            (recur (conj paths p))))))
    ; change the path `p` of transitions into a string
    ; this string is a string recognized by the dfa
    ; limitation: the :chars must be a sequence of one element
    (process-path [p]
      (loop [result [] p1 p]
        (if (= 1 (count p1))
          (apply str result)
          (recur
            (conj result (-> p1 first :chars first))
            (rest p1)))))]
    (let [stt (cstt dfa)
          paths (find-paths stt (dfa :start))
          strings (map #(process-path %) paths)]
      strings)))

; WORKS in clojure 1.7.0 only for the second and third test
; returns full collection (not a lazy collection)
; will get stuck for tests 4, 5, 6
(defn gen-strings2 [dfa]
  (letfn [
    ; create state transition table from the deterministic finite automaton `dfa`
    (cstt [dfa]
      (letfn [
        ; is the state final
        (is-final? [state] (contains? (dfa :accepts) state))
        ; create state transition table row
        (csttr [dfa current]
          (letfn [
            (collect-states [dfa current]
              (let [trans ((dfa :transitions) current)]
                (loop [alps (keys trans) row []]
                  (if (empty? alps)
                    row
                    (let [k (first alps)]
                      (recur (rest alps) (conj row [(trans k) k])))))))
            (merge-values [row]
              (loop [r row result {}]
                (if (empty? r)
                  result
                  (let [[state event] (first r)]
                    (recur 
                      (rest r)
                      (update-in result [state] conj event))))))
            (process-merged-row [row]
              (if (empty? row)
                row
                (update row (-> row keys first) reverse)))
            (create-row [merged-row current]
              (if (empty? merged-row)
                [{:cs current
                 :final (is-final? current)}]
                (loop [mr merged-row sttr []]
                  (if (empty? mr)
                    sttr
                    (recur 
                      (rest mr) 
                      (conj sttr {:ns (key (first mr))
                                  :chars (val (first mr))
                                  :cs current
                                  :final (is-final? current)}))))))]
            (create-row (process-merged-row (merge-values (collect-states dfa current))) current)))]
      (loop [states (sort (dfa :states)) result []]
        (if (empty? states)
          result
          (recur (rest states) (conj result (csttr dfa (first states))))))))
    ; find transition to the state `state` where the states status `state-status` is either :cs or :ns
    (find-trans [stt state state-status]
      (loop [stt1 stt]
        (when-not (empty? stt1)
          (let [st (-> stt1 first first)]
            (if (= (st state-status) state)
              (first stt1)
              (recur (rest stt1)))))))
    ; find transitions to current state
    (fttcs [stt state] (find-trans stt state :cs))
    ; find transitions to next state
    (fttns [stt state] (find-trans stt state :ns))
    ; return true if the `path` contains path `p`
    (contains-path? [p paths] (not (empty? (filter #(= % p) paths))))
    ; remove paths from transitions
    ; removing alrady explored paths from transitions enables finding different / new paths
    (rpft [transitions paths]
      (letfn [
        (remove-trans [transitions trans] (remove #(= trans %) transitions))
        ; remove single path from transitions
        ; from the `transitions` remove the single path `p`
        (rspft [transitions p]
          (if-not (> (count transitions) 1)
            transitions
            (loop [transitions1 transitions p1 p]
              (if (empty? p1)
                transitions1
                (recur (remove-trans transitions1 (first p1)) (rest p1))))))]
      (loop [transitions1 transitions paths1 paths]
        (if (empty? paths1)
          transitions1
          (recur (rspft transitions1 (first paths1)) (rest paths1))))))
    ; find path in the `stt` to the state `state`
    ; the `current-path` must be an atom into which transitions leading to a final state (including) are stored
    (find-path [stt state current-path paths]
      ; (println "----------------")
      ; (println "state: " state)
      ; (println "current-path: " @current-path)
      ; (println "paths: " paths)
      ; (println "transitions: " (fttcs stt state))
      (loop [transitions (rpft (fttcs stt state) paths)]
        (if (empty? transitions)
          @current-path
          (let [trans (first transitions)
                _ (swap! current-path conj trans)
                cont-path (contains-path? @current-path paths)
                final-state (-> trans :final)]
            (cond 
              ; not a final state - continue
              (not final-state)                       (find-path stt (trans :ns) current-path paths)
              ; it is a final state and is not in the collection of already found states
              (and final-state (not cont-path))       (recur (rest transitions))
              ;it is an already found final state and it is possible to go further - so go further
              (and final-state cont-path (trans :ns)) (find-path stt (trans :ns) current-path paths)
              ;found everything we could - stop
              :default                                (recur (rest transitions)))))))
    ; find all paths leading to all final states in the state transition table `stt`
    ; starting from the starting state `start-state`
    (find-paths [stt start-state]
      (loop [paths []]
        (let [p (find-path stt start-state (atom []) paths)]
          (if (or (empty? p) (contains-path? p paths))
            paths
            (recur (conj paths p))))))
    ; change the path `p` of transitions into a string
    ; this string is a string recognized by the dfa
    (process-path [p]
      (letfn [
        (create-string [chars-list]
          (if (empty? chars-list)
            '(())
            (for [x (first chars-list) more (create-string (rest chars-list))]
              (cons x more))))]
      (let [chars-list (filter #(not= % nil) (map #(% :chars) p))]
        (map #(apply str %) (create-string chars-list)))))]
    (let [stt (cstt dfa)
          paths (find-paths stt (dfa :start))
          strings (flatten (map #(process-path %) paths))]
      strings)))

; WORKS in clojure 1.7.0 for first, second and third test
; for 4th, 5th and 6th test returns paths without loops (as if loops were removed from the dfa), but it will not get stuck on the loops
; returns full collection (not a lazy collection)
(defn gen-strings3 [dfa]
  (letfn [
    ; create state transition table from the deterministic finite automaton `dfa`
    (cstt [dfa]
      (letfn [
        ; is the state final
        (is-final? [state] (contains? (dfa :accepts) state))
        ; create state transition table row
        (csttr [dfa current]
          (letfn [
            (collect-states [dfa current]
              (let [trans ((dfa :transitions) current)]
                (loop [alps (keys trans) row []]
                  (if (empty? alps)
                    row
                    (let [k (first alps)]
                      (recur (rest alps) (conj row [(trans k) k])))))))
            (merge-values [row]
              (loop [r row result {}]
                (if (empty? r)
                  result
                  (let [[state event] (first r)]
                    (recur 
                      (rest r)
                      (update-in result [state] conj event))))))
            (process-merged-row [row]
              (if (empty? row)
                row
                (update row (-> row keys first) reverse)))
            (create-row [merged-row current]
              (if (empty? merged-row)
                [{:cs current
                 :final (is-final? current)}]
                (loop [mr merged-row sttr []]
                  (if (empty? mr)
                    sttr
                    (recur 
                      (rest mr) 
                      (conj sttr {:ns (key (first mr))
                                  :chars (val (first mr))
                                  :cs current
                                  :final (is-final? current)}))))))]
            (create-row (process-merged-row (merge-values (collect-states dfa current))) current)))]
      (loop [states (sort (dfa :states)) result []]
        (if (empty? states)
          result
          (recur (rest states) (conj result (csttr dfa (first states))))))))
    ; find transition to the state `state` where the states status `state-status` is either :cs or :ns
    (find-trans [stt state state-status]
      (loop [stt1 stt]
        (when-not (empty? stt1)
          (let [st (-> stt1 first first)]
            (if (= (st state-status) state)
              (first stt1)
              (recur (rest stt1)))))))
    ; find transitions to current state
    (fttcs [stt state] (find-trans stt state :cs))
    ; find transitions to next state
    (fttns [stt state] (find-trans stt state :ns))
    ; return true if the `paths` contains path `p`
    (contains-path? [p paths] (not (empty? (filter #(= % p) paths))))
    ; remove paths from transitions
    ; removing alredy explored paths from transitions enables finding different / new paths
    (rpft [transitions paths]
      (letfn [
        (remove-trans [transitions trans] (remove #(= trans %) transitions))
        ; remove single path from transitions
        ; from the `transitions` remove the single path `p`
        (rspft [transitions p]
          (if-not (> (count transitions) 1)
            transitions
            (loop [transitions1 transitions p1 p]
              (if (empty? p1)
                transitions1
                (recur (remove-trans transitions1 (first p1)) (rest p1))))))]
      (loop [transitions1 transitions paths1 paths]
        (if (empty? paths1)
          transitions1
          (recur (rspft transitions1 (first paths1)) (rest paths1))))))
    ; return true if if the input transition `trans` is  a simple loop (the current and the next states are same)
    (simple-loop? [trans] (= (:cs trans) (:ns trans)))
    ; remove simple loop transitions
    ; from the `transitions` remove simple loop transitions
    (rslt [transitions] (filter #(-> % simple-loop? not) transitions))
    ; is path part of comlex loop ?
    ; return true if the path `is` is a part of the input `complex-loop` 
    (ippocl [p complex-loop]
      (loop [rev-path (reverse p) rev-cl (reverse complex-loop) common []]
        (if (or (empty? rev-path) (empty? rev-cl))
          (>= (count common) 2)
          (if (= (first rev-path) (first rev-cl))
            (recur (rest rev-path) (rest rev-cl) (conj common (first rev-path)))
            (recur (rest rev-path) (rest rev-cl) common)))))
    ; is path part of any complex loops ?
    (ippoacl [p complex-loops] (-> (filter true? (map #(ippocl p %) complex-loops)) empty? not))
    ; find path in the `stt` to the state `state`
    ; the `current-path` must be an atom into which transitions leading to a final state (including) are stored
    (find-path [stt state current-path paths complex-loops]
      ; (println "----------------")
      ; (println "stt: " stt)
      ; (println "state: " state)
      ; (println "current-path1: " @current-path)
      ; (println "paths: " paths)
      ; (println "complex-loops: " complex-loops)
      ; (println "transitions1: " (fttcs stt state))
      ; (println "transitions2: " (rpft (fttcs stt state) paths))
      ; (println "transitions3: " (rslt (rpft (fttcs stt state) paths)))
      (loop [transitions (rslt (rpft (fttcs stt state) paths))]
        (if (empty? transitions)
          @current-path
          (let [trans (first transitions)
                ; _ (println "trans: " trans)
                empty-curr-path (empty? @current-path)
                final-state     (:final trans)
                contains-path   (contains-path? @current-path paths)
                can-continue    (-> trans :ns nil? not)
                part-comp-loop  (ippoacl @current-path complex-loops)
                ; _ (println "final-state:     " final-state)
                ; _ (println "contains-path:   " contains-path)
                ; _ (println "empty-curr-path: " empty-curr-path)
                ; _ (println "can-continue:    " can-continue)
                ; _ (println part-comp-loop)
                condition1 (not final-state)
                condition2 (and final-state (not part-comp-loop) can-continue empty-curr-path)
                condition3 (and final-state (not part-comp-loop) contains-path can-continue)
                ; _ (println "condition1: " condition1)
                ; _ (println "condition2: " condition2)
                ; _ (println "condition3: " condition3)
                ]
            (if (or condition1 condition2 condition3)
              (do
                (swap! current-path conj trans)
                (find-path stt (trans :ns) current-path paths complex-loops))
              (recur (rest transitions)))))))
    ; find all paths leading to all final states in the state transition table `stt`
    ; starting from the starting state `start-state`
    ; the `complex-loops` contains all complex loop in the `stt` if they exists
    (find-paths [stt start-state complex-loops]
      (loop [paths []]
        (let [p (find-path stt start-state (atom []) paths complex-loops)]
          (if (or (empty? p) (contains-path? p paths))
            paths
            (recur (conj paths p))))))
    ; retrieve complex loop for final state
    ; for input `trans` find a complex loop which contains it
    ; assumption - a single final state can be part of exactly one complex loop (otherwise this function will not work)
    (rclffs [stt trans]
      ; if the transition is for non final state, or the state does not have the next state - no complex loop
      (if (or (-> trans :final not) (-> trans :ns not))
        '()
        (loop [transition trans explored []]
          (if (or (nil? transition) (and (-> explored empty? not) (= (:cs transition) (-> explored first :cs))))
            explored
            (let [next-transition (first (rslt (fttcs stt (:ns transition))))]
              (recur next-transition (conj explored transition)))))))
    ; retrieve complex loops
    ; for all final states in the `dfa` find all complex loop which are they part of
    ; assumption - a final state has only one transition
    (rcl [final-states stt]
      (loop [finals final-states result []]
        (if (empty? finals)
          (filter #(-> % empty? not) result)
          (recur 
            (rest finals) 
            (conj result (rclffs stt (first (rslt (fttcs stt (first finals))))))))))
    ; retrieve simple loops
    ; get set of simple loops (current-state and next-state are same) for input `cstt`
    ; if no such loop exists, returned set will be empty
    (rsl [stt]
      (loop [stt1 stt result []]
        (if (empty? stt1)
          (-> result flatten set)
          (let [filtered (filter #(= (:cs %) (:ns %)) (first stt1))]
            (recur (rest stt1) (conj result filtered))))))
    ; change the path `p` of transitions into a string
    ; this string is a string recognized by the dfa
    (process-path [p]
      (letfn [
        (create-string [chars-list]
          (if (empty? chars-list)
            '(())
            (for [x (first chars-list) more (create-string (rest chars-list))]
              (cons x more))))]
      (let [chars-list (filter #(not= % nil) (map #(% :chars) p))]
        (map #(apply str %) (create-string chars-list)))))]
    (let [stt (cstt dfa)
          complex-loops (rcl (:accepts dfa) stt)
          simple-loops (rsl stt)
          paths (find-paths stt (dfa :start) complex-loops)
          strings (flatten (map #(process-path %) paths))]
      strings)))

; WORKS in clojure 1.7.0 for 1st, 2nd, 3rd, 5th and 6th tests
; The 4th test fails - Caused by: java.lang.StackOverflowError: null - at java.util.regex.Pattern$Loop.match (Pattern.java:4785)
(defn gen-strings4 [dfa]
  (letfn [
    ; Create State Transition Table from the deterministic finite automaton `dfa`
    (cstt [dfa]
      (letfn [
        ; is the state final
        (is-final? [state] (contains? (dfa :accepts) state))
        ; create state transition table row
        (csttr [dfa current]
          (letfn [
            (collect-states [dfa current]
              (let [trans ((dfa :transitions) current)]
                (loop [alps (keys trans) row []]
                  (if (empty? alps)
                    row
                    (let [k (first alps)]
                      (recur (rest alps) (conj row [(trans k) k])))))))
            (merge-values [row]
              (loop [r row result {}]
                (if (empty? r)
                  result
                  (let [[state event] (first r)]
                    (recur 
                      (rest r)
                      (update-in result [state] conj event))))))
            (process-merged-row [row]
              (let [k (-> row keys first)]
                (if (empty? row)
                  row
                  (assoc row k (reverse (get row k))))))
            (create-row [merged-row current]
              (if (empty? merged-row)
                [{:cs current
                 :final (is-final? current)}]
                (loop [mr merged-row sttr []]
                  (if (empty? mr)
                    sttr
                    (recur 
                      (rest mr) 
                      (conj sttr {:ns (key (first mr))
                                  :chars (val (first mr))
                                  :cs current
                                  :final (is-final? current)}))))))]
            (create-row (process-merged-row (merge-values (collect-states dfa current))) current)))]
      (loop [states (sort (dfa :states)) result []]
        (if (empty? states)
          result
          (recur (rest states) (conj result (csttr dfa (first states))))))))
    ; find transition to the state `state` where the states status `state-status` is either :cs or :ns
    (find-trans [stt state state-status]
      (loop [stt1 stt]
        (when-not (empty? stt1)
          (let [st (-> stt1 first first)]
            (if (= (st state-status) state)
              (first stt1)
              (recur (rest stt1)))))))
    ; Find Transitions To Current State
    (fttcs [stt state] (find-trans stt state :cs))
    ; Find Transitions To Next State
    (fttns [stt state] (find-trans stt state :ns))
    ; return true if the `paths` contains path `p`
    (contains-path? [p paths] (not (empty? (filter #(= % p) paths))))
    ; Remove Paths From Transitions
    ; removing alredy explored paths from transitions enables finding different / new paths
    (rpft [transitions paths]
      (letfn [
        (remove-trans [transitions trans] (remove #(= trans %) transitions))
        ; remove single path from transitions
        ; from the `transitions` remove the single path `p`
        (rspft [transitions p]
          (if-not (> (count transitions) 1)
            transitions
            (loop [transitions1 transitions p1 p]
              (if (empty? p1)
                transitions1
                (recur (remove-trans transitions1 (first p1)) (rest p1))))))]
      (loop [transitions1 transitions paths1 paths]
        (if (empty? paths1)
          transitions1
          (recur (rspft transitions1 (first paths1)) (rest paths1))))))
    ; return true if if the input transition `trans` is  a simple loop (the current and the next states are same)
    (simple-loop? [trans] (= (:cs trans) (:ns trans)))
    ; Remove Simple Loop Transitions
    ; from the `transitions` remove simple loop transitions
    (rslt [transitions] (filter #(-> % simple-loop? not) transitions))
    ; Is Path Part Of Comlex Loop ?
    ; return true if the path `is` is a part of the input `complex-loop` 
    (ippocl [p complex-loop]
      (loop [rev-path (reverse p) rev-cl (reverse complex-loop) common []]
        (if (or (empty? rev-path) (empty? rev-cl))
          (>= (count common) 2)
          (if (= (first rev-path) (first rev-cl))
            (recur (rest rev-path) (rest rev-cl) (conj common (first rev-path)))
            (recur (rest rev-path) (rest rev-cl) common)))))
    ; Is Path Part Of Any Complex Loops ?
    (ippoacl [p complex-loops] (-> (filter true? (map #(ippocl p %) complex-loops)) empty? not))
    ; Is Path Part Of Simple Loop ?
    (ipposl [p simple-loop] (= 1 (count (filter #(= (:cs simple-loop) (:cs %)) p))))
    ; is input loop a simple loop ?
    (is-simple-loop?  [loopp] (map? loopp))
    ; is input loop a complex loop ?
    (is-complex-loop? [loopp] (vector? loopp))
    ; Is Path Part Of Loop ?
    (ippol [p loopp]
      (if (is-simple-loop? loopp)
        (ipposl p loopp)
        (ippocl p loopp)))
    ; find path in the `stt` to the state `state`
    ; the `current-path` must be an atom into which transitions leading to a final state (including) are stored
    (find-path [stt state current-path paths complex-loops]
      (loop [transitions (rslt (rpft (fttcs stt state) paths))]
        (if (empty? transitions)
          @current-path
          (let [trans (first transitions)
                empty-curr-path (empty? @current-path)
                final-state     (:final trans)
                contains-path   (contains-path? @current-path paths)
                can-continue    (-> trans :ns nil? not)
                part-comp-loop  (ippoacl @current-path complex-loops)
                condition1 (not final-state)
                condition2 (and final-state (not part-comp-loop) can-continue empty-curr-path)
                condition3 (and final-state (not part-comp-loop) contains-path can-continue)]
            (if (or condition1 condition2 condition3)
              (do
                (swap! current-path conj trans)
                (find-path stt (trans :ns) current-path paths complex-loops))
              (recur (rest transitions)))))))
    ; find all paths leading to all final states in the state transition table `stt`
    ; starting from the starting state `start-state`
    ; the `complex-loops` contains all complex loop in the `stt` if they exists
    (find-paths [stt start-state complex-loops]
      (loop [paths []]
        (let [p (find-path stt start-state (atom []) paths complex-loops)]
          (if (or (empty? p) (contains-path? p paths))
            paths
            (recur (conj paths p))))))
    ; Retrieve Complex Loops For Final State
    ; for input `trans` find a complex loop which contains it
    ; assumption - a single final state can be part of exactly one complex loop
    (rclffs [stt trans]
      ; if the transition is for non final state, or the state does not have the next state - no complex loop
      (if (or (-> trans :final not) (-> trans :ns not))
        '()
        (loop [transition trans explored []]
          (if (or (nil? transition) (and (-> explored empty? not) (= (:cs transition) (-> explored first :cs))))
            (if (-> explored last :ns)
              explored
              '())
            (let [next-transition (first (rslt (fttcs stt (:ns transition))))]
              (recur next-transition (conj explored transition)))))))
    ; Retrieve Complex Loops
    ; for all final states in the `dfa` find all complex loop which are they part of
    ; assumption - a final state has only one transition
    (rcl [final-states stt]
      (loop [finals final-states result []]
        (if (empty? finals)
          (filter #(-> % empty? not) result)
          (recur 
            (rest finals) 
            (conj result (rclffs stt (first (rslt (fttcs stt (first finals))))))))))
    ; Retrieve Simple Loops
    ; get set of simple loops (current-state and next-state are same) for input `cstt`
    ; if no such loop exists, returned set will be empty
    (rsl [stt]
      (loop [stt1 stt result []]
        (if (empty? stt1)
          (-> result flatten set)
          (let [filtered (filter #(= (:cs %) (:ns %)) (first stt1))]
            (recur (rest stt1) (conj result filtered))))))
    ; change the path `p` of transitions into a string
    ; this string is a string recognized by the dfa
    (process-path [p]
      (let [chars-list (filter #(not= % nil) (map #(% :chars) p))]
      (letfn [
        (has-only-one-char? [chars-list] (empty? (filter #(> % 1) (map count chars-list))))
        (create-string-from-multiple-char-lists [chars-list]
          (if (empty? chars-list)
            '(())
            (for [x (first chars-list) more (create-string-from-multiple-char-lists(rest chars-list))]
              (cons x more))))]
      (if (has-only-one-char? chars-list)
          (clojure.string/join (flatten chars-list))
          (map #(apply str %) (create-string-from-multiple-char-lists chars-list))))))
    ; generate path from input path `p` and `loop`
    ; argument `n` expresses the length of the loop part
    (generate-path [loopp n p]
      (letfn [
        ; generate path from a `simple-loop` and "normal" path `p`
        (generate-path-a [simple-loop n p]
          (let [find-pos  (fn [state p]
                            (loop [p1 p i 0]
                              (if (= state (-> p1 first :cs))
                                i
                                (recur (rest p1) (inc i)))))
                pos (find-pos (:cs simple-loop) p)
                generated (repeat n simple-loop)]
            (cond
              (zero? pos)              (concat generated p)
              (= pos (-> p count dec)) (concat p generated)
              :default 
                (let [splitted (split-at pos p)]
                  (concat (first splitted) generated (last splitted))))))
        ; generate path from a `complex-loop`
        (generate-path-b [complex-loop n p] (into p (flatten (repeat n complex-loop))))]
      (if (is-simple-loop? loopp)       
        (generate-path-a loopp n p)
        (generate-path-b loopp n p))))
    ; generate lazy-seq of paths by using a normal path `p` and list of loops in the dfa `loops`
    ; assumption: input path `p` is part of all loops in `loops` (can not happen that is part of first but not second loop)
    (generate-paths
      ([p loops]
        (if (empty? loops)
          '()
          (generate-paths p loops 1 0)))
      ([p loops n idx] 
        (cons 
          (generate-path (loops idx) n p)
          (lazy-seq (generate-paths p loops (if (zero? idx) (inc n) n) (if (= idx (-> loops count dec)) 0 (inc idx))))
        )))
    ; Filter Out Paths Without Loops
    ; From the input list of paths `remove` remove those paths which are not part of any loop in the input `loops` list
    (fopwl [paths loops] 
      (if (empty? loops)
        '()
        (loop [loops1 loops result #{}]
          (if (empty? loops1)
            result
            (recur (rest loops1) (into result (filter #(-> (ippol % (first loops1))) paths)))))))]
    (let [stt (cstt dfa)
          simple-loops (vec (rsl stt))
          complex-loops (rcl (:accepts dfa) stt)
          loops (vec (filter #(not-empty %) (into simple-loops complex-loops)))
          paths (find-paths stt (dfa :start) complex-loops)
          paths-with-loops (fopwl paths loops)
          lazy-paths (generate-paths (first paths-with-loops) loops) ;assumption - always only one path is part of any loop
          strings (flatten (map #(process-path %) paths))]
      (flatten (map #(process-path %) (lazy-cat paths lazy-paths))))))

; WORKS in clojure 1.7.0
; GOT timeout on the 4clojure site
; TOOK 56 seconds for all tests to pass on local computer
(defn gen-strings5 [dfa]
  (letfn [
    ; Create State Transition Table from the deterministic finite automaton `dfa`
    (cstt [dfa]
      (letfn [
        ; is the state final
        (is-final? [state] (contains? (dfa :accepts) state))
        ; create state transition table row
        (csttr [dfa current]
          (letfn [
            (collect-states [dfa current]
              (let [trans ((dfa :transitions) current)]
                (loop [alps (keys trans) row []]
                  (if (empty? alps)
                    row
                    (let [k (first alps)]
                      (recur (rest alps) (conj row [(trans k) k])))))))
            (merge-values [row]
              (loop [r row result {}]
                (if (empty? r)
                  result
                  (let [[state event] (first r)]
                    (recur 
                      (rest r)
                      (update-in result [state] conj event))))))
            (process-merged-row [row]
              (let [k (-> row keys first)]
                (if (empty? row)
                  row
                  (assoc row k (reverse (get row k))))))
            (create-row [merged-row current]
              (if (empty? merged-row)
                [{:cs current
                 :final (is-final? current)}]
                (loop [mr merged-row sttr []]
                  (if (empty? mr)
                    sttr
                    (recur 
                      (rest mr) 
                      (conj sttr {:ns (key (first mr))
                                  :chars (val (first mr))
                                  :cs current
                                  :final (is-final? current)}))))))]
            (create-row (process-merged-row (merge-values (collect-states dfa current))) current)))]
      (loop [states (sort (dfa :states)) result []]
        (if (empty? states)
          result
          (recur (rest states) (conj result (csttr dfa (first states))))))))
    ; find transition to the state `state` where the states status `state-status` is either :cs or :ns
    (find-trans [stt state state-status]
      (loop [stt1 stt]
        (when-not (empty? stt1)
          (let [st (-> stt1 first first)]
            (if (= (st state-status) state)
              (first stt1)
              (recur (rest stt1)))))))
    ; Find Transitions To Current State
    (fttcs [stt state] (find-trans stt state :cs))
    ; Find Transitions To Next State
    (fttns [stt state] (find-trans stt state :ns))
    ; return true if the `paths` contains path `p`
    (contains-path? [p paths] (not (empty? (filter #(= % p) paths))))
    ; Remove Paths From Transitions
    ; removing alredy explored paths from transitions enables finding different / new paths
    (rpft [transitions paths]
      (letfn [
        (remove-trans [transitions trans] (remove #(= trans %) transitions))
        ; remove single path from transitions
        ; from the `transitions` remove the single path `p`
        (rspft [transitions p]
          (if-not (> (count transitions) 1)
            transitions
            (loop [transitions1 transitions p1 p]
              (if (empty? p1)
                transitions1
                (recur (remove-trans transitions1 (first p1)) (rest p1))))))]
      (loop [transitions1 transitions paths1 paths]
        (if (empty? paths1)
          transitions1
          (recur (rspft transitions1 (first paths1)) (rest paths1))))))
    ; return true if if the input transition `trans` is  a simple loop (the current and the next states are same)
    (simple-loop? [trans] (= (:cs trans) (:ns trans)))
    ; Remove Simple Loop Transitions
    ; from the `transitions` remove simple loop transitions
    (rslt [transitions] (filter #(-> % simple-loop? not) transitions))
    ; Is Path Part Of Comlex Loop ?
    ; return true if the path `is` is a part of the input `complex-loop` 
    (ippocl [p complex-loop]
      (loop [rev-path (reverse p) rev-cl (reverse complex-loop) common []]
        (if (or (empty? rev-path) (empty? rev-cl))
          (>= (count common) 2)
          (if (= (first rev-path) (first rev-cl))
            (recur (rest rev-path) (rest rev-cl) (conj common (first rev-path)))
            (recur (rest rev-path) (rest rev-cl) common)))))
    ; Is Path Part Of Any Complex Loops ?
    (ippoacl [p complex-loops] (-> (filter true? (map #(ippocl p %) complex-loops)) empty? not))
    ; Is Path Part Of Simple Loop ?
    (ipposl [p simple-loop] (= 1 (count (filter #(= (:cs simple-loop) (:cs %)) p))))
    ; is input loop a simple loop ?
    (is-simple-loop?  [loopp] (map? loopp))
    ; is input loop a complex loop ?
    (is-complex-loop? [loopp] (vector? loopp))
    ; Is Path Part Of Loop ?
    (ippol [p loopp]
      (if (is-simple-loop? loopp)
        (ipposl p loopp)
        (ippocl p loopp)))
    ; find path in the `stt` to the state `state`
    ; the `current-path` must be an atom into which transitions leading to a final state (including) are stored
    (find-path [stt state current-path paths complex-loops]
      (loop [transitions (rslt (rpft (fttcs stt state) paths))]
        (if (empty? transitions)
          @current-path
          (let [trans (first transitions)
                empty-curr-path (empty? @current-path)
                final-state     (:final trans)
                contains-path   (contains-path? @current-path paths)
                can-continue    (-> trans :ns nil? not)
                part-comp-loop  (ippoacl @current-path complex-loops)
                condition1 (not final-state)
                condition2 (and final-state (not part-comp-loop) can-continue empty-curr-path)
                condition3 (and final-state (not part-comp-loop) contains-path can-continue)]
            (if (or condition1 condition2 condition3)
              (do
                (swap! current-path conj trans)
                (find-path stt (trans :ns) current-path paths complex-loops))
              (recur (rest transitions)))))))
    ; find all paths leading to all final states in the state transition table `stt`
    ; starting from the starting state `start-state`
    ; the `complex-loops` contains all complex loop in the `stt` if they exists
    (find-paths [stt start-state complex-loops]
      (loop [paths []]
        (let [p (find-path stt start-state (atom []) paths complex-loops)]
          (if (or (empty? p) (contains-path? p paths))
            paths
            (recur (conj paths p))))))
    ; Retrieve Complex Loops For Final State
    ; for input `trans` find a complex loop which contains it
    ; assumption - a single final state can be part of exactly one complex loop
    (rclffs [stt trans]
      ; if the transition is for non final state, or the state does not have the next state - no complex loop
      (if (or (-> trans :final not) (-> trans :ns not))
        '()
        (loop [transition trans explored []]
          (if (or (nil? transition) (and (-> explored empty? not) (= (:cs transition) (-> explored first :cs))))
            (if (-> explored last :ns)
              explored
              '())
            (let [next-transition (first (rslt (fttcs stt (:ns transition))))]
              (recur next-transition (conj explored transition)))))))
    ; Retrieve Complex Loops
    ; for all final states in the `dfa` find all complex loop which are they part of
    ; assumption - a final state has only one transition
    (rcl [final-states stt]
      (loop [finals final-states result []]
        (if (empty? finals)
          (filter #(-> % empty? not) result)
          (recur 
            (rest finals) 
            (conj result (rclffs stt (first (rslt (fttcs stt (first finals))))))))))
    ; Retrieve Simple Loops
    ; get set of simple loops (current-state and next-state are same) for input `cstt`
    ; if no such loop exists, returned set will be empty
    (rsl [stt]
      (loop [stt1 stt result []]
        (if (empty? stt1)
          (-> result flatten set)
          (let [filtered (filter #(= (:cs %) (:ns %)) (first stt1))]
            (recur (rest stt1) (conj result filtered))))))
    ; change the path `p` of transitions into a string
    ; this string is a string recognized by the dfa
    (process-path [p]
      (let [chars-list (filter #(not= % nil) (map #(% :chars) p))]
      (letfn [
        (has-only-one-char? [chars-list] (empty? (filter #(> % 1) (map count chars-list))))
        (create-string-from-multiple-char-lists [chars-list]
          (if (empty? chars-list)
            '(())
            (for [x (first chars-list) more (create-string-from-multiple-char-lists(rest chars-list))]
              (cons x more))))]
      (if (has-only-one-char? chars-list)
          (clojure.string/join (flatten chars-list))
          (map #(apply str %) (create-string-from-multiple-char-lists chars-list))))))
    ; generate path from input path `p` and `loop`
    ; argument `n` expresses the length of the loop part
    (generate-path [loopp n p]
      (letfn [
        ; generate path from a `simple-loop` and "normal" path `p`
        (generate-path-a [simple-loop n p]
          (let [find-pos  (fn [state p]
                            (loop [p1 p i 0]
                              (if (= state (-> p1 first :cs))
                                i
                                (recur (rest p1) (inc i)))))
                pos (find-pos (:cs simple-loop) p)
                generated (repeat n simple-loop)]
            (cond
              (zero? pos)              (concat generated p)
              (= pos (-> p count dec)) (concat p generated)
              :default 
                (let [splitted (split-at pos p)]
                  (concat (first splitted) generated (last splitted))))))
        ; generate path from a `complex-loop`
        (generate-path-b [complex-loop n p] (into p (flatten (repeat n complex-loop))))]
      (if (is-simple-loop? loopp)       
        (generate-path-a loopp n p)
        (generate-path-b loopp n p))))
    ; generate lazy-seq of paths by using a normal path `p` and list of loops in the dfa `loops`
    ; assumption: input path `p` is part of all loops in `loops` (can not happen that is part of first but not second loop)
    (generate-paths
      ([p loops]
        (if (empty? loops)
          '()
          (generate-paths p loops 1 0)))
      ([p loops n idx] 
        (cons 
          (generate-path (loops idx) n p)
          (lazy-seq (generate-paths p loops (if (zero? idx) (inc n) n) (if (= idx (-> loops count dec)) 0 (inc idx))))
        )))
    ; Filter Out Paths Without Loops
    ; From the input list of paths `remove` remove those paths which are not part of any loop in the input `loops` list
    (fopwl [paths loops] 
      (if (empty? loops)
        '()
        (loop [loops1 loops result #{}]
          (if (empty? loops1)
            result
            (recur (rest loops1) (into result (filter #(-> (ippol % (first loops1))) paths)))))))]
    (let [stt (cstt dfa)
          simple-loops (vec (rsl stt))
          complex-loops (rcl (:accepts dfa) stt)
          loops (vec (filter #(not-empty %) (into simple-loops complex-loops)))
          paths (find-paths stt (dfa :start) complex-loops)]
      (if (empty? loops)
        (flatten (map #(process-path %) paths))
        (let [
          paths-with-loops (fopwl paths loops)
          ; using all loops for generating strings fails due to
          ; java.lang.StackOverflowError: null - at java.util.regex.Pattern$Loop.match (Pattern.java:4785)
          ; taking one loop from list of all loops, and using it for generating works - so be it
          ; lazy-paths (generate-paths (first paths-with-loops) loops)] 
          lazy-paths (generate-paths (first paths-with-loops) [(first loops)])]     ; assumption - always only one path is part of any loop
          (flatten (map #(process-path %) (lazy-cat paths lazy-paths))))))))

; WORKS in clojure 1.7.0
; GOT timeout on the 4clojure site
; TOOK 44 seconds for all tests to pass on local computer
(defn gen-strings6 [dfa]
  (letfn [
    ; Create State Transition Table from the deterministic finite automaton `dfa`
    (cstt [dfa]
      (letfn [
        ; is the state final
        (is-final? [state] (contains? (dfa :accepts) state))
        ; create state transition table row
        (csttr [dfa current]
          (letfn [
            (collect-states [dfa current]
              (let [trans ((dfa :transitions) current)]
                (loop [alps (keys trans) row []]
                  (if (empty? alps)
                    row
                    (let [k (first alps)]
                      (recur (rest alps) (conj row [(trans k) k])))))))
            (merge-values [row]
              (loop [r row result {}]
                (if (empty? r)
                  result
                  (let [[state event] (first r)]
                    (recur 
                      (rest r)
                      (update-in result [state] conj event))))))
            (process-merged-row [row]
              (let [k (-> row keys first)]
                (if (empty? row)
                  row
                  (assoc row k (reverse (get row k))))))
            (create-row [merged-row current]
              (if (empty? merged-row)
                [{:cs current
                 :final (is-final? current)}]
                (loop [mr merged-row sttr []]
                  (if (empty? mr)
                    sttr
                    (recur 
                      (rest mr) 
                      (conj sttr {:ns (key (first mr))
                                  :chars (val (first mr))
                                  :cs current
                                  :final (is-final? current)}))))))]
            (create-row (process-merged-row (merge-values (collect-states dfa current))) current)))]
      (loop [states (sort (dfa :states)) result []]
        (if (empty? states)
          result
          (recur (rest states) (conj result (csttr dfa (first states))))))))
    ; find transition to the state `state` where the states status `state-status` is either :cs or :ns
    (find-trans [stt state state-status]
      (loop [stt1 stt]
        (when-not (empty? stt1)
          (let [st (-> stt1 first first)]
            (if (= (st state-status) state)
              (first stt1)
              (recur (rest stt1)))))))
    ; Find Transitions To Current State
    (fttcs [stt state] (find-trans stt state :cs))
    ; Find Transitions To Next State
    (fttns [stt state] (find-trans stt state :ns))
    ; return true if the `paths` contains path `p`
    (contains-path? [p paths] (not (empty? (filter #(= % p) paths))))
    ; Remove Paths From Transitions
    ; removing alredy explored paths from transitions enables finding different / new paths
    (rpft [transitions paths]
      (letfn [
        (remove-trans [transitions trans] (remove #(= trans %) transitions))
        ; remove single path from transitions
        ; from the `transitions` remove the single path `p`
        (rspft [transitions p]
          (if-not (> (count transitions) 1)
            transitions
            (loop [transitions1 transitions p1 p]
              (if (empty? p1)
                transitions1
                (recur (remove-trans transitions1 (first p1)) (rest p1))))))]
      (loop [transitions1 transitions paths1 paths]
        (if (empty? paths1)
          transitions1
          (recur (rspft transitions1 (first paths1)) (rest paths1))))))
    ; return true if if the input transition `trans` is  a simple loop (the current and the next states are same)
    (simple-loop? [trans] (= (:cs trans) (:ns trans)))
    ; Remove Simple Loop Transitions
    ; from the `transitions` remove simple loop transitions
    (rslt [transitions] (filter #(-> % simple-loop? not) transitions))
    ; Is Path Part Of Comlex Loop ?
    ; return true if the path `is` is a part of the input `complex-loop` 
    (ippocl [p complex-loop]
      (loop [rev-path (reverse p) rev-cl (reverse complex-loop) common []]
        (if (or (empty? rev-path) (empty? rev-cl))
          (>= (count common) 2)
          (if (= (first rev-path) (first rev-cl))
            (recur (rest rev-path) (rest rev-cl) (conj common (first rev-path)))
            (recur (rest rev-path) (rest rev-cl) common)))))
    ; Is Path Part Of Any Complex Loops ?
    (ippoacl [p complex-loops] (-> (filter true? (map #(ippocl p %) complex-loops)) empty? not))
    ; Is Path Part Of Simple Loop ?
    (ipposl [p simple-loop] (= 1 (count (filter #(= (:cs simple-loop) (:cs %)) p))))
    ; is input loop a simple loop ?
    (is-simple-loop?  [loopp] (map? loopp))
    ; is input loop a complex loop ?
    (is-complex-loop? [loopp] (vector? loopp))
    ; Is Path Part Of Loop ?
    (ippol [p loopp]
      (if (is-simple-loop? loopp)
        (ipposl p loopp)
        (ippocl p loopp)))
    ; find path in the `stt` to the state `state`
    ; the `current-path` must be an atom into which transitions leading to a final state (including) are stored
    (find-path [stt state current-path paths complex-loops]
      (loop [transitions (rslt (rpft (fttcs stt state) paths))]
        (if (empty? transitions)
          @current-path
          (let [trans (first transitions)
                empty-curr-path (empty? @current-path)
                final-state     (:final trans)
                contains-path   (contains-path? @current-path paths)
                can-continue    (-> trans :ns nil? not)
                part-comp-loop  (ippoacl @current-path complex-loops)
                condition1 (not final-state)
                condition2 (and final-state (not part-comp-loop) can-continue empty-curr-path)
                condition3 (and final-state (not part-comp-loop) contains-path can-continue)]
            (if (or condition1 condition2 condition3)
              (do
                (swap! current-path conj trans)
                (find-path stt (trans :ns) current-path paths complex-loops))
              (recur (rest transitions)))))))
    ; find all paths leading to all final states in the state transition table `stt`
    ; starting from the starting state `start-state`
    ; the `complex-loops` contains all complex loop in the `stt` if they exists
    (find-paths [stt start-state complex-loops]
      (loop [paths []]
        (let [p (find-path stt start-state (atom []) paths complex-loops)]
          (if (or (empty? p) (contains-path? p paths))
            paths
            (recur (conj paths p))))))
    ; Retrieve Complex Loops For Final State
    ; for input `trans` find a complex loop which contains it
    ; assumption - a single final state can be part of exactly one complex loop
    (rclffs [stt trans]
      ; if the transition is for non final state, or the state does not have the next state - no complex loop
      (if (or (-> trans :final not) (-> trans :ns not))
        '()
        (loop [transition trans explored []]
          (if (or (nil? transition) (and (-> explored empty? not) (= (:cs transition) (-> explored first :cs))))
            (if (-> explored last :ns)
              explored
              '())
            (let [next-transition (first (rslt (fttcs stt (:ns transition))))]
              (recur next-transition (conj explored transition)))))))
    ; Retrieve Complex Loops
    ; for all final states in the `dfa` find all complex loop which are they part of
    ; assumption - a final state has only one transition
    (rcl [final-states stt]
      (loop [finals final-states result []]
        (if (empty? finals)
          (filter #(-> % empty? not) result)
          (recur 
            (rest finals) 
            (conj result (rclffs stt (first (rslt (fttcs stt (first finals))))))))))
    ; Retrieve Simple Loops
    ; get set of simple loops (current-state and next-state are same) for input `cstt`
    ; if no such loop exists, returned set will be empty
    (rsl [stt]
      (loop [stt1 stt result []]
        (if (empty? stt1)
          (-> result flatten set)
          (let [filtered (filter #(= (:cs %) (:ns %)) (first stt1))]
            (recur (rest stt1) (conj result filtered))))))
    ; change the path `p` of transitions into a string
    ; this string is a string recognized by the dfa
    (process-path [p]
      (let [chars-list (filter #(not= % nil) (map #(% :chars) p))]
      (letfn [
        (has-only-one-char? [chars-list] (empty? (filter #(> % 1) (map count chars-list))))
        (create-string-from-multiple-char-lists [chars-list]
          (if (empty? chars-list)
            '(())
            (for [x (first chars-list) more (create-string-from-multiple-char-lists(rest chars-list))]
              (cons x more))))]
      (if (has-only-one-char? chars-list)
          (clojure.string/join (flatten chars-list))
          (map #(apply str %) (create-string-from-multiple-char-lists chars-list))))))
    ; generate path from input path `p` and `loop`
    ; using the previously generated path `prev-gen-path`
    (generate-path [p loopp prev-gen-path]
      (letfn [
        ; generate path from a `simple-loop` and "normal" path `p`
        (generate-path-a [p simple-loop]
          (let [find-pos  (fn [state p]
                            (loop [p1 p i 0]
                              (if (= state (-> p1 first :cs))
                                i
                                (recur (rest p1) (inc i)))))
                pos (find-pos (:cs simple-loop) p)]
            (cond
              (zero? pos)              (concat [simple-loop] p)
              (= pos (-> p count dec)) (concat p [simple-loop])
              :default 
                (let [splitted (split-at pos p)]
                  (concat (first splitted) [simple-loop] (last splitted))))))]
      (cond 
        (and (-> prev-gen-path nil?)     (-> loopp is-simple-loop?))      (generate-path-a p loopp)
        (and (-> prev-gen-path nil?)     (-> loopp is-simple-loop? not))  (into p loopp)
        (and (-> prev-gen-path nil? not) (-> loopp is-simple-loop?))      (generate-path-a prev-gen-path loopp)
        (and (-> prev-gen-path nil? not) (-> loopp is-simple-loop? not))  (into p prev-gen-path))))
    ; generate lazy-seq of paths by using a normal path `p` and the loop `loop`
    (generate-paths 
      ([p loopp] (generate-paths p loopp (generate-path p loopp nil)))
      ([p loopp generated]
        (cons 
          generated
          (lazy-seq (generate-paths p loopp (generate-path p loopp generated))))))
    ; Filter Out Paths Without Loops
    ; From the input list of paths `paths` remove those paths which are not part of any loop in the input `loops` list
    (fopwl [paths loops] 
      (if (empty? loops)
        '()
        (loop [loops1 loops result #{}]
          (if (empty? loops1)
            result
            (recur (rest loops1) (into result (filter #(-> (ippol % (first loops1))) paths)))))))]
    (let [stt (cstt dfa)
          simple-loops (vec (rsl stt))
          complex-loops (rcl (:accepts dfa) stt)
          loops (vec (filter #(not-empty %) (into simple-loops complex-loops)))
          paths (find-paths stt (dfa :start) complex-loops)]
      (if (empty? loops)
        (flatten (map #(process-path %) paths))
        (let [
          paths-with-loops (fopwl paths loops)
          lazy-paths (generate-paths (first paths-with-loops) (first loops))]
          (flatten (map #(process-path %) (lazy-cat paths lazy-paths))))))))

; WORKS in clojure 1.7.0
; TOOK 20 seconds for all tests to pass on local computer
(defn gen-strings7 [dfa]
  (letfn [
    ; Create State Transition Table from the deterministic finite automaton `dfa`
    (cstt [dfa]
      (letfn [
        ; is the state final
        (is-final? [state] (contains? (dfa :accepts) state))
        ; create state transition table row
        (csttr [dfa current]
          (letfn [
            (collect-states [dfa current]
              (let [trans ((dfa :transitions) current)]
                (loop [alps (keys trans) row []]
                  (if (empty? alps)
                    row
                    (let [k (first alps)]
                      (recur (rest alps) (conj row [(trans k) k])))))))
            (merge-values [row]
              (loop [r row result {}]
                (if (empty? r)
                  result
                  (let [[state event] (first r)]
                    (recur 
                      (rest r)
                      (update-in result [state] conj event))))))
            (process-merged-row [row]
              (let [k (-> row keys first)]
                (if (empty? row)
                  row
                  (assoc row k (reverse (get row k))))))
            (create-row [merged-row current]
              (if (empty? merged-row)
                [{:cs current
                 :final (is-final? current)}]
                (loop [mr merged-row sttr []]
                  (if (empty? mr)
                    sttr
                    (recur 
                      (rest mr) 
                      (conj sttr {:ns (key (first mr))
                                  :chars (val (first mr))
                                  :cs current
                                  :final (is-final? current)}))))))]
            (create-row (process-merged-row (merge-values (collect-states dfa current))) current)))]
      (loop [states (sort (dfa :states)) result []]
        (if (empty? states)
          result
          (recur (rest states) (conj result (csttr dfa (first states))))))))
    ; find transition to the state `state` where the states status `state-status` is either :cs or :ns
    (find-trans [stt state state-status]
      (loop [stt1 stt]
        (when-not (empty? stt1)
          (let [st (-> stt1 first first)]
            (if (= (st state-status) state)
              (first stt1)
              (recur (rest stt1)))))))
    ; Find Transitions To Current State
    (fttcs [stt state] (find-trans stt state :cs))
    ; Find Transitions To Next State
    (fttns [stt state] (find-trans stt state :ns))
    ; return true if the `paths` contains path `p`
    (contains-path? [p paths] (not (empty? (filter #(= % p) paths))))
    ; Remove Paths From Transitions
    ; removing alredy explored paths from transitions enables finding different / new paths
    (rpft [transitions paths]
      (letfn [
        (remove-trans [transitions trans] (remove #(= trans %) transitions))
        ; remove single path from transitions
        ; from the `transitions` remove the single path `p`
        (rspft [transitions p]
          (if-not (> (count transitions) 1)
            transitions
            (loop [transitions1 transitions p1 p]
              (if (empty? p1)
                transitions1
                (recur (remove-trans transitions1 (first p1)) (rest p1))))))]
      (loop [transitions1 transitions paths1 paths]
        (if (empty? paths1)
          transitions1
          (recur (rspft transitions1 (first paths1)) (rest paths1))))))
    ; return true if if the input transition `trans` is  a simple loop (the current and the next states are same)
    (simple-loop? [trans] (= (:cs trans) (:ns trans)))
    ; Remove Simple Loop Transitions
    ; from the `transitions` remove simple loop transitions
    (rslt [transitions] (filter #(-> % simple-loop? not) transitions))
    ; Is Path Part Of Comlex Loop ?
    ; return true if the path `is` is a part of the input `complex-loop` 
    (ippocl [p complex-loop]
      (loop [rev-path (reverse p) rev-cl (reverse complex-loop) common []]
        (if (or (empty? rev-path) (empty? rev-cl))
          (>= (count common) 2)
          (if (= (first rev-path) (first rev-cl))
            (recur (rest rev-path) (rest rev-cl) (conj common (first rev-path)))
            (recur (rest rev-path) (rest rev-cl) common)))))
    ; Is Path Part Of Any Complex Loops ?
    (ippoacl [p complex-loops] (-> (filter true? (map #(ippocl p %) complex-loops)) empty? not))
    ; Is Path Part Of Simple Loop ?
    (ipposl [p simple-loop] (= 1 (count (filter #(= (:cs simple-loop) (:cs %)) p))))
    ; is input loop a simple loop ?
    (is-simple-loop?  [loopp] (map? loopp))
    ; is input loop a complex loop ?
    (is-complex-loop? [loopp] (vector? loopp))
    ; Is Path Part Of Loop ?
    (ippol [p loopp]
      (if (is-simple-loop? loopp)
        (ipposl p loopp)
        (ippocl p loopp)))
    ; find path in the `stt` to the state `state`
    ; the `current-path` must be an atom into which transitions leading to a final state (including) are stored
    (find-path [stt state current-path paths complex-loops]
      (loop [transitions (rslt (rpft (fttcs stt state) paths))]
        (if (empty? transitions)
          @current-path
          (let [trans (first transitions)
                empty-curr-path (empty? @current-path)
                final-state     (:final trans)
                contains-path   (contains-path? @current-path paths)
                can-continue    (-> trans :ns nil? not)
                part-comp-loop  (ippoacl @current-path complex-loops)
                condition1 (not final-state)
                condition2 (and final-state (not part-comp-loop) can-continue empty-curr-path)
                condition3 (and final-state (not part-comp-loop) contains-path can-continue)]
            (if (or condition1 condition2 condition3)
              (do
                (swap! current-path conj trans)
                (find-path stt (trans :ns) current-path paths complex-loops))
              (recur (rest transitions)))))))
    ; find all paths leading to all final states in the state transition table `stt`
    ; starting from the starting state `start-state`
    ; the `complex-loops` contains all complex loop in the `stt` if they exists
    (find-paths [stt start-state complex-loops]
      (loop [paths []]
        (let [p (find-path stt start-state (atom []) paths complex-loops)]
          (if (or (empty? p) (contains-path? p paths))
            paths
            (recur (conj paths p))))))
    ; Retrieve Complex Loops For Final State
    ; for input `trans` find a complex loop which contains it
    ; assumption - a single final state can be part of exactly one complex loop
    (rclffs [stt trans]
      ; if the transition is for non final state, or the state does not have the next state - no complex loop
      (if (or (-> trans :final not) (-> trans :ns not))
        '()
        (loop [transition trans explored []]
          (if (or (nil? transition) (and (-> explored empty? not) (= (:cs transition) (-> explored first :cs))))
            (if (-> explored last :ns)
              explored
              '())
            (let [next-transition (first (rslt (fttcs stt (:ns transition))))]
              (recur next-transition (conj explored transition)))))))
    ; Retrieve Complex Loops
    ; for all final states in the `dfa` find all complex loop which are they part of
    ; assumption - a final state has only one transition
    (rcl [final-states stt]
      (loop [finals final-states result []]
        (if (empty? finals)
          (filter #(-> % empty? not) result)
          (recur 
            (rest finals) 
            (conj result (rclffs stt (first (rslt (fttcs stt (first finals))))))))))
    ; Retrieve Simple Loops
    ; get set of simple loops (current-state and next-state are same) for input `cstt`
    ; if no such loop exists, returned set will be empty
    (rsl [stt]
      (loop [stt1 stt result []]
        (if (empty? stt1)
          (-> result flatten set)
          (let [filtered (filter #(= (:cs %) (:ns %)) (first stt1))]
            (recur (rest stt1) (conj result filtered))))))
    ; change the path `p` of transitions into a string
    ; this string is a string recognized by the dfa
    (process-path [p]
      (let [chars-list (filter #(not= % nil) (map #(% :chars) p))]
      (letfn [
        (has-only-one-char? [chars-list] (empty? (filter #(> % 1) (map count chars-list))))
        (create-string-from-multiple-char-lists [chars-list]
          (if (empty? chars-list)
            '(())
            (for [x (first chars-list) more (create-string-from-multiple-char-lists(rest chars-list))]
              (cons x more))))]
      (if (has-only-one-char? chars-list)
          (clojure.string/join (flatten chars-list))
          (map #(apply str %) (create-string-from-multiple-char-lists chars-list))))))
    ; generate string either by using input path `p` and `loop`
    ; or previously generated string `prev-gen-string`
    (generate-string [p loopp path-as-str prev-gen-string n]
      (letfn [
        ; generate path from a `simple-loop` and "normal" path `p`
        (generate-path-a [simple-loop n p]
          (let [find-pos  (fn [state p]
                            (loop [p1 p i 0]
                              (if (= state (-> p1 first :cs))
                                i
                                (recur (rest p1) (inc i)))))
                pos (find-pos (:cs simple-loop) p)
                generated (repeat n simple-loop)]
            (cond
              (zero? pos)              (concat generated p)
              (= pos (-> p count dec)) (concat p generated)
              :default 
                (let [splitted (split-at pos p)]
                  (concat (first splitted) generated (last splitted))))))]
      (cond 
        (and (-> prev-gen-string nil?)     (-> loopp is-simple-loop?))      (process-path (generate-path-a loopp n p))
        (and (-> prev-gen-string nil?)     (-> loopp is-simple-loop? not))  (process-path (into p loopp))
        (and (-> prev-gen-string nil? not) (-> loopp is-simple-loop?))      (process-path (generate-path-a loopp n p))
        (and (-> prev-gen-string nil? not) (-> loopp is-simple-loop? not))  (str path-as-str prev-gen-string))))
    ; generate lazy-seq of strings by using a normal path `p` and the loop `loop`
    (generate-strings 
      ([p loopp path-as-str] (generate-strings p loopp path-as-str (generate-string p loopp nil nil 1) 2))
      ([p loopp path-as-str generated n]
        (cons 
          generated
          (lazy-seq (generate-strings p loopp path-as-str (generate-string p loopp path-as-str generated n) (inc n))))))
    ; Filter Out Paths Without Loops
    ; From the input list of paths `paths` remove those paths which are not part of any loop in the input `loops` list
    (fopwl [paths loops] 
      (if (empty? loops)
        '()
        (loop [loops1 loops result #{}]
          (if (empty? loops1)
            result
            (recur (rest loops1) (into result (filter #(-> (ippol % (first loops1))) paths)))))))]
    (let [stt (cstt dfa)
          simple-loops (vec (rsl stt))
          complex-loops (rcl (:accepts dfa) stt)
          loops (vec (filter #(not-empty %) (into simple-loops complex-loops)))
          paths (find-paths stt (dfa :start) complex-loops)
          strings (flatten (map #(process-path %) paths))]
      (if (empty? loops)
        strings
        (let [
          paths-with-loops (fopwl paths loops)
          single-path (first paths-with-loops)
          path-as-str (process-path single-path)
          lazy-gen-strings (generate-strings single-path (first loops) path-as-str)]
          (lazy-cat strings lazy-gen-strings))))))

; WORKS in clojure 1.7.0
; TOOK 2.5 seconds for all tests to pass on local computer
(defn gen-strings8 [dfa]
  (letfn [
    ; Create State Transition Table from the deterministic finite automaton `dfa`
    (cstt [dfa]
      (letfn [
        ; is the state final
        (is-final? [state] (contains? (dfa :accepts) state))
        ; create state transition table row
        (csttr [dfa current]
          (letfn [
            (collect-states [dfa current]
              (let [trans ((dfa :transitions) current)]
                (loop [alps (keys trans) row []]
                  (if (empty? alps)
                    row
                    (let [k (first alps)]
                      (recur (rest alps) (conj row [(trans k) k])))))))
            (merge-values [row]
              (loop [r row result {}]
                (if (empty? r)
                  result
                  (let [[state event] (first r)]
                    (recur 
                      (rest r)
                      (update-in result [state] conj event))))))
            (process-merged-row [row]
              (let [k (-> row keys first)]
                (if (empty? row)
                  row
                  (assoc row k (reverse (get row k))))))
            (create-row [merged-row current]
              (if (empty? merged-row)
                [{:cs current
                 :final (is-final? current)}]
                (loop [mr merged-row sttr []]
                  (if (empty? mr)
                    sttr
                    (recur 
                      (rest mr) 
                      (conj sttr {:ns (key (first mr))
                                  :chars (val (first mr))
                                  :cs current
                                  :final (is-final? current)}))))))]
            (create-row (process-merged-row (merge-values (collect-states dfa current))) current)))]
      (loop [states (sort (dfa :states)) result []]
        (if (empty? states)
          result
          (recur (rest states) (conj result (csttr dfa (first states))))))))
    ; find transition to the state `state` where the states status `state-status` is either :cs or :ns
    (find-trans [stt state state-status]
      (loop [stt1 stt]
        (when-not (empty? stt1)
          (let [st (-> stt1 first first)]
            (if (= (st state-status) state)
              (first stt1)
              (recur (rest stt1)))))))
    ; Find Transitions To Current State
    (fttcs [stt state] (find-trans stt state :cs))
    ; Find Transitions To Next State
    (fttns [stt state] (find-trans stt state :ns))
    ; return true if the `paths` contains path `p`
    (contains-path? [p paths] (not (empty? (filter #(= % p) paths))))
    ; Remove Paths From Transitions
    ; removing alredy explored paths from transitions enables finding different / new paths
    (rpft [transitions paths]
      (letfn [
        (remove-trans [transitions trans] (remove #(= trans %) transitions))
        ; remove single path from transitions
        ; from the `transitions` remove the single path `p`
        (rspft [transitions p]
          (if-not (> (count transitions) 1)
            transitions
            (loop [transitions1 transitions p1 p]
              (if (empty? p1)
                transitions1
                (recur (remove-trans transitions1 (first p1)) (rest p1))))))]
      (loop [transitions1 transitions paths1 paths]
        (if (empty? paths1)
          transitions1
          (recur (rspft transitions1 (first paths1)) (rest paths1))))))
    ; return true if if the input transition `trans` is  a simple loop (the current and the next states are same)
    (simple-loop? [trans] (= (:cs trans) (:ns trans)))
    ; Remove Simple Loop Transitions
    ; from the `transitions` remove simple loop transitions
    (rslt [transitions] (filter #(-> % simple-loop? not) transitions))
    ; Is Path Part Of Comlex Loop ?
    ; return true if the path `is` is a part of the input `complex-loop` 
    (ippocl [p complex-loop]
      (loop [rev-path (reverse p) rev-cl (reverse complex-loop) common []]
        (if (or (empty? rev-path) (empty? rev-cl))
          (>= (count common) 2)
          (if (= (first rev-path) (first rev-cl))
            (recur (rest rev-path) (rest rev-cl) (conj common (first rev-path)))
            (recur (rest rev-path) (rest rev-cl) common)))))
    ; Is Path Part Of Any Complex Loops ?
    (ippoacl [p complex-loops] (-> (filter true? (map #(ippocl p %) complex-loops)) empty? not))
    ; Is Path Part Of Simple Loop ?
    (ipposl [p simple-loop] (= 1 (count (filter #(= (:cs simple-loop) (:cs %)) p))))
    ; is input loop a simple loop ?
    (is-simple-loop?  [loopp] (map? loopp))
    ; is input loop a complex loop ?
    (is-complex-loop? [loopp] (vector? loopp))
    ; Is Path Part Of Loop ?
    (ippol [p loopp]
      (if (is-simple-loop? loopp)
        (ipposl p loopp)
        (ippocl p loopp)))
    ; find path in the `stt` to the state `state`
    ; the `current-path` must be an atom into which transitions leading to a final state (including) are stored
    (find-path [stt state current-path paths complex-loops]
      (loop [transitions (rslt (rpft (fttcs stt state) paths))]
        (if (empty? transitions)
          @current-path
          (let [trans (first transitions)
                empty-curr-path (empty? @current-path)
                final-state     (:final trans)
                contains-path   (contains-path? @current-path paths)
                can-continue    (-> trans :ns nil? not)
                part-comp-loop  (ippoacl @current-path complex-loops)
                condition1 (not final-state)
                condition2 (and final-state (not part-comp-loop) can-continue empty-curr-path)
                condition3 (and final-state (not part-comp-loop) contains-path can-continue)]
            (if (or condition1 condition2 condition3)
              (do
                (swap! current-path conj trans)
                (find-path stt (trans :ns) current-path paths complex-loops))
              (recur (rest transitions)))))))
    ; find all paths leading to all final states in the state transition table `stt`
    ; starting from the starting state `start-state`
    ; the `complex-loops` contains all complex loop in the `stt` if they exists
    (find-paths [stt start-state complex-loops]
      (loop [paths []]
        (let [p (find-path stt start-state (atom []) paths complex-loops)]
          (if (or (empty? p) (contains-path? p paths))
            paths
            (recur (conj paths p))))))
    ; Retrieve Complex Loops For Final State
    ; for input `trans` find a complex loop which contains it
    ; assumption - a single final state can be part of exactly one complex loop
    (rclffs [stt trans]
      ; if the transition is for non final state, or the state does not have the next state - no complex loop
      (if (or (-> trans :final not) (-> trans :ns not))
        '()
        (loop [transition trans explored []]
          (if (or (nil? transition) (and (-> explored empty? not) (= (:cs transition) (-> explored first :cs))))
            (if (-> explored last :ns)
              explored
              '())
            (let [next-transition (first (rslt (fttcs stt (:ns transition))))]
              (recur next-transition (conj explored transition)))))))
    ; Retrieve Complex Loops
    ; for all final states in the `dfa` find all complex loop which are they part of
    ; assumption - a final state has only one transition
    (rcl [final-states stt]
      (loop [finals final-states result []]
        (if (empty? finals)
          (filter #(-> % empty? not) result)
          (recur 
            (rest finals) 
            (conj result (rclffs stt (first (rslt (fttcs stt (first finals))))))))))
    ; Retrieve Simple Loops
    ; get set of simple loops (current-state and next-state are same) for input `cstt`
    ; if no such loop exists, returned set will be empty
    (rsl [stt]
      (loop [stt1 stt result []]
        (if (empty? stt1)
          (-> result flatten set)
          (let [filtered (filter #(= (:cs %) (:ns %)) (first stt1))]
            (recur (rest stt1) (conj result filtered))))))
    ; change the path `p` of transitions into a string
    ; this string is a string recognized by the dfa
    (process-path [p]
      (let [chars-list (filter #(not= % nil) (map #(% :chars) p))]
      (letfn [
        (has-only-one-char? [chars-list] (empty? (filter #(> % 1) (map count chars-list))))
        (create-string-from-multiple-char-lists [chars-list]
          (if (empty? chars-list)
            '(())
            (for [x (first chars-list) more (create-string-from-multiple-char-lists(rest chars-list))]
              (cons x more))))]
      (if (has-only-one-char? chars-list)
          (clojure.string/join (flatten chars-list))
          (map #(apply str %) (create-string-from-multiple-char-lists chars-list))))))
    ; Find Position For Simple Loop
    ; Find the position for inserting a `simple-loop` in a path `p`
    (fpfsl [simple-loop p]
      (when (is-simple-loop? simple-loop)
        (letfn [(find-pos [state p]
                  (loop [p1 p i 0]
                    (if (= state (-> p1 first :cs))
                      i
                      (recur (rest p1) (inc i)))))]
          (find-pos (:cs simple-loop) p))))
    ; generate string either by using input path `p` and `loop`
    ; or previously generated string `prev-gen-string`
    (generate-string [p loopp path-as-str prev-gen-string n slp]
      (letfn [
        ; generate path from a `simple-loop` and "normal" path `p`
        (generate-path [simple-loop n p pos]
          (let [generated (repeat n simple-loop)]
            (cond
              (zero? pos)              (concat generated p)
              (= pos (-> p count dec)) (concat p generated)
              :default 
                (let [splitted (split-at pos p)]
                  (concat (first splitted) generated (last splitted))))))
        ; generate string from previous generated string `prev-gen-string` and position `pos`
        ; into which insert a char `c`
        (generate-string-int [prev-gen-string pos c]
          (cond
            (zero? pos)              (str c prev-gen-string)
            (= pos (-> p count dec)) (str prev-gen-string c)
            :default 
              (str (subs prev-gen-string 0 pos) c (subs prev-gen-string (inc pos)))))]
      (cond 
        (and (-> prev-gen-string nil?)     (-> loopp is-simple-loop?))      (process-path (generate-path loopp n p slp))
        (and (-> prev-gen-string nil?)     (-> loopp is-simple-loop? not))  (process-path (into p loopp))
        (and (-> prev-gen-string nil? not) (-> loopp is-simple-loop?))      (generate-string-int prev-gen-string slp (-> loopp :chars first))
        (and (-> prev-gen-string nil? not) (-> loopp is-simple-loop? not))  (str path-as-str prev-gen-string))))
    ; generate lazy-seq of strings by using a normal path `p` and the loop `loop`
    (generate-strings 
      ([p loopp path-as-str] 
        (let [pos (fpfsl loopp p)]
          (generate-strings p loopp path-as-str (generate-string p loopp nil nil 1 pos) 2 pos)))
      ([p loopp path-as-str generated n pos]
        (cons 
          generated
          (lazy-seq (generate-strings p loopp path-as-str (generate-string p loopp path-as-str generated n pos) (inc n) pos)))))
    ; Filter Out Paths Without Loops
    ; From the input list of paths `paths` remove those paths which are not part of any loop in the input `loops` list
    (fopwl [paths loops] 
      (if (empty? loops)
        '()
        (loop [loops1 loops result #{}]
          (if (empty? loops1)
            result
            (recur (rest loops1) (into result (filter #(-> (ippol % (first loops1))) paths)))))))]
    (let [stt (cstt dfa)
          simple-loops (vec (rsl stt))
          complex-loops (rcl (:accepts dfa) stt)
          loops (vec (filter #(not-empty %) (into simple-loops complex-loops)))
          paths (find-paths stt (dfa :start) complex-loops)
          strings (flatten (map #(process-path %) paths))]
      (if (empty? loops)
        strings
        (let [
          paths-with-loops (fopwl paths loops)
          single-path (first paths-with-loops)
          path-as-str (process-path single-path)
          lazy-gen-strings (generate-strings single-path (first loops) path-as-str)]
          (lazy-cat strings lazy-gen-strings))))))

; PASSES tests on 4clojure site 
; (thanks Leif for pointing out that 4clojure site is using clojure 1.4.0)
; WORKS both in clojure 1.7.0 and 1.4.0
; TOOK 3.0 seconds for all tests to pass on local computer
(defn gen-strings9 [dfa]
  (letfn [
    ; Create State Transition Table from the deterministic finite automaton `dfa`
    (cstt [dfa]
      (letfn [
        ; is the state final
        (is-final? [state] (contains? (dfa :accepts) state))
        ; create state transition table row
        (csttr [dfa current]
          (letfn [
            (collect-states [dfa current]
              (let [trans ((dfa :transitions) current)]
                (loop [alps (keys trans) row []]
                  (if (empty? alps)
                    row
                    (let [k (first alps)]
                      (recur (rest alps) (conj row [(trans k) k])))))))
            (merge-values [row]
              (loop [r row result {}]
                (if (empty? r)
                  result
                  (let [[state event] (first r)]
                    (recur 
                      (rest r)
                      (update-in result [state] conj event))))))
            (process-merged-row [row]
              (let [k (-> row keys first)]
                (if (empty? row)
                  row
                  (assoc row k (reverse (get row k))))))
            (create-row [merged-row current]
              (if (empty? merged-row)
                [{:cs current
                 :final (is-final? current)}]
                (loop [mr merged-row sttr []]
                  (if (empty? mr)
                    sttr
                    (recur 
                      (rest mr) 
                      (conj sttr {:ns (key (first mr))
                                  :chars (val (first mr))
                                  :cs current
                                  :final (is-final? current)}))))))
            (sort-row [row] (vec (sort-by #(-> % :ns str) row)))]
            (sort-row (create-row (process-merged-row (merge-values (collect-states dfa current))) current))))]
      (loop [states (sort (dfa :states)) result []]
        (if (empty? states)
          result
          (recur (rest states) (conj result (csttr dfa (first states))))))))
    ; find transition to the state `state` where the states status `state-status` is either :cs or :ns
    (find-trans [stt state state-status]
      (loop [stt1 stt]
        (when-not (empty? stt1)
          (let [st (-> stt1 first first)]
            (if (= (st state-status) state)
              (first stt1)
              (recur (rest stt1)))))))
    ; Find Transitions To Current State
    (fttcs [stt state] (find-trans stt state :cs))
    ; Find Transitions To Next State
    (fttns [stt state] (find-trans stt state :ns))
    ; return true if the `paths` contains path `p`
    (contains-path? [p paths] (not (empty? (filter #(= % p) paths))))
    ; Remove Paths From Transitions
    ; removing alredy explored paths from transitions enables finding different / new paths
    (rpft [transitions paths]
      (letfn [
        (remove-trans [transitions trans] (remove #(= trans %) transitions))
        ; remove single path from transitions
        ; from the `transitions` remove the single path `p`
        (rspft [transitions p]
          (if-not (> (count transitions) 1)
            transitions
            (loop [transitions1 transitions p1 p]
              (if (empty? p1)
                transitions1
                (recur (remove-trans transitions1 (first p1)) (rest p1))))))]
      (loop [transitions1 transitions paths1 paths]
        (if (empty? paths1)
          transitions1
          (recur (rspft transitions1 (first paths1)) (rest paths1))))))
    ; return true if if the input transition `trans` is  a simple loop (the current and the next states are same)
    (simple-loop? [trans] (= (:cs trans) (:ns trans)))
    ; Remove Simple Loop Transitions
    ; from the `transitions` remove simple loop transitions
    (rslt [transitions] (filter #(-> % simple-loop? not) transitions))
    ; Is Path Part Of Comlex Loop ?
    ; return true if the path `is` is a part of the input `complex-loop` 
    (ippocl [p complex-loop]
      (loop [rev-path (reverse p) rev-cl (reverse complex-loop) common []]
        (if (or (empty? rev-path) (empty? rev-cl))
          (>= (count common) 2)
          (if (= (first rev-path) (first rev-cl))
            (recur (rest rev-path) (rest rev-cl) (conj common (first rev-path)))
            (recur (rest rev-path) (rest rev-cl) common)))))
    ; Is Path Part Of Any Complex Loops ?
    (ippoacl [p complex-loops] (-> (filter true? (map #(ippocl p %) complex-loops)) empty? not))
    ; Is Path Part Of Simple Loop ?
    (ipposl [p simple-loop] (= 1 (count (filter #(= (:cs simple-loop) (:cs %)) p))))
    ; is input loop a simple loop ?
    (is-simple-loop?  [loopp] (map? loopp))
    ; is input loop a complex loop ?
    (is-complex-loop? [loopp] (vector? loopp))
    ; Is Path Part Of Loop ?
    (ippol [p loopp]
      (if (is-simple-loop? loopp)
        (ipposl p loopp)
        (ippocl p loopp)))
    ; find path in the `stt` to the state `state`
    ; the `current-path` must be an atom into which transitions leading to a final state (including) are stored
    (find-path [stt state current-path paths complex-loops]
      (loop [transitions (rslt (rpft (fttcs stt state) paths))]
        (if (empty? transitions)
          @current-path
          (let [trans (first transitions)
                empty-curr-path (empty? @current-path)
                final-state     (:final trans)
                contains-path   (contains-path? @current-path paths)
                can-continue    (-> trans :ns nil? not)
                part-comp-loop  (ippoacl @current-path complex-loops)
                condition1 (not final-state)
                condition2 (and final-state (not part-comp-loop) can-continue empty-curr-path)
                condition3 (and final-state (not part-comp-loop) contains-path can-continue)]
            (if (or condition1 condition2 condition3)
              (do
                (swap! current-path conj trans)
                (find-path stt (trans :ns) current-path paths complex-loops))
              (recur (rest transitions)))))))
    ; find all paths leading to all final states in the state transition table `stt`
    ; starting from the starting state `start-state`
    ; the `complex-loops` contains all complex loop in the `stt` if they exists
    (find-paths [stt start-state complex-loops]
      (loop [paths []]
        (let [p (find-path stt start-state (atom []) paths complex-loops)]
          (if (or (empty? p) (contains-path? p paths))
            paths
            (recur (conj paths p))))))
    ; Retrieve Complex Loops For Final State
    ; for input `trans` find a complex loop which contains it
    ; assumption - a single final state can be part of exactly one complex loop
    (rclffs [stt trans]
      ; if the transition is for non final state, or the state does not have the next state - no complex loop
      (if (or (-> trans :final not) (-> trans :ns not))
        '()
        (loop [transition trans explored []]
          (if (or (nil? transition) (and (-> explored empty? not) (= (:cs transition) (-> explored first :cs))))
            (if (-> explored last :ns)
              explored
              '())
            (let [next-transition (first (rslt (fttcs stt (:ns transition))))]
              (recur next-transition (conj explored transition)))))))
    ; Retrieve Complex Loops
    ; for all final states in the `dfa` find all complex loop which are they part of
    ; assumption - a final state has only one transition
    (rcl [final-states stt]
      (loop [finals final-states result []]
        (if (empty? finals)
          (filter #(-> % empty? not) result)
          (recur 
            (rest finals) 
            (conj result (rclffs stt (first (rslt (fttcs stt (first finals))))))))))
    ; Retrieve Simple Loops
    ; get set of simple loops (current-state and next-state are same) for input `c-stt`
    ; if no such loop exists, returned set will be empty
    (rsl [stt]
      (loop [stt1 stt result []]
        (if (empty? stt1)
          (-> result flatten set)
          (let [filtered (filter #(= (:cs %) (:ns %)) (first stt1))]
            (recur (rest stt1) (conj result filtered))))))
    ; change the path `p` of transitions into a string
    ; this string is a string recognized by the dfa
    (process-path [p]
      (let [chars-list (filter #(not= % nil) (map #(% :chars) p))]
      (letfn [
        (has-only-one-char? [chars-list] (empty? (filter #(> % 1) (map count chars-list))))
        (create-string-from-multiple-char-lists [chars-list]
          (if (empty? chars-list)
            '(())
            (for [x (first chars-list) more (create-string-from-multiple-char-lists(rest chars-list))]
              (cons x more))))]
      (if (has-only-one-char? chars-list)
          (clojure.string/join (flatten chars-list))
          (map #(apply str %) (create-string-from-multiple-char-lists chars-list))))))
    ; Find Position For Simple Loop
    ; Find the position for inserting a `simple-loop` in a path `p`
    (fpfsl [simple-loop p]
      (when (is-simple-loop? simple-loop)
        (letfn [(find-pos [state p]
                  (loop [p1 p i 0]
                    (if (= state (-> p1 first :cs))
                      i
                      (recur (rest p1) (inc i)))))]
          (find-pos (:cs simple-loop) p))))
    ; generate string either by using input path `p` and `loop`
    ; or previously generated string `prev-gen-string`
    (generate-string [p loopp path-as-str prev-gen-string n slp]
      (letfn [
        ; generate path from a `simple-loop` and "normal" path `p`
        (generate-path [simple-loop n p pos]
          (let [generated (repeat n simple-loop)]
            (cond
              (zero? pos)              (concat generated p)
              (= pos (-> p count dec)) (concat p generated)
              :default 
                (let [splitted (split-at pos p)]
                  (concat (first splitted) generated (last splitted))))))
        ; generate string from previous generated string `prev-gen-string` and position `pos`
        ; into which insert a char `c`
        (generate-string-int [prev-gen-string pos c]
          (cond
            (zero? pos)              (str c prev-gen-string)
            (= pos (-> p count dec)) (str prev-gen-string c)
            :default 
              (str (subs prev-gen-string 0 pos) c (subs prev-gen-string (inc pos)))))]
      (cond 
        (and (-> prev-gen-string nil?)     (-> loopp is-simple-loop?))      (process-path (generate-path loopp n p slp))
        (and (-> prev-gen-string nil?)     (-> loopp is-simple-loop? not))  (process-path (into p loopp))
        (and (-> prev-gen-string nil? not) (-> loopp is-simple-loop?))      (generate-string-int prev-gen-string slp (-> loopp :chars first))
        (and (-> prev-gen-string nil? not) (-> loopp is-simple-loop? not))  (str path-as-str prev-gen-string))))
    ; generate lazy-seq of strings by using a normal path `p` and the loop `loop`
    (generate-strings 
      ([p loopp path-as-str] 
        (let [pos (fpfsl loopp p)]
          (generate-strings p loopp path-as-str (generate-string p loopp nil nil 1 pos) 2 pos)))
      ([p loopp path-as-str generated n pos]
        (cons 
          generated
          (lazy-seq (generate-strings p loopp path-as-str (generate-string p loopp path-as-str generated n pos) (inc n) pos)))))
    ; Filter Out Paths Without Loops
    ; From the input list of paths `paths` remove those paths which are not part of any loop in the input `loops` list
    (fopwl [paths loops] 
      (if (empty? loops)
        '()
        (loop [loops1 loops result #{}]
          (if (empty? loops1)
            result
            (recur (rest loops1) (into result (filter #(-> (ippol % (first loops1))) paths)))))))]
    (let [stt (cstt dfa)
          simple-loops (vec (rsl stt))
          complex-loops (rcl (:accepts dfa) stt)
          loops (vec (filter #(not-empty %) (into simple-loops complex-loops)))
          paths (find-paths stt (dfa :start) complex-loops)
          strings (flatten (map #(process-path %) paths))]
      (if (empty? loops)
        strings
        (let [
          paths-with-loops (fopwl paths loops)
          single-path (first paths-with-loops)
          path-as-str (process-path single-path)
          lazy-gen-strings (generate-strings single-path (first loops) path-as-str)]
          (lazy-cat strings lazy-gen-strings))))))

;; Tests

(println (= #{"a" "ab" "abc"}
   (set (gen-strings9 '{:states #{q0 q1 q2 q3}
                        :alphabet #{a b c}
                        :start q0
                        :accepts #{q1 q2 q3}
                        :transitions {q0 {a q1}
                                     q1 {b q2}
                                     q2 {c q3}}}))))

(println (= #{"hi" "hey" "hello"}
   (set (gen-strings9 '{:states #{q0 q1 q2 q3 q4 q5 q6 q7}
                        :alphabet #{e h i l o y}
                        :start q0
                        :accepts #{q2 q4 q7}
                        :transitions {q0 {h q1}
                            q1 {i q2, e q3}
                            q3 {l q5, y q4}
                            q5 {l q6}
                            q6 {o q7}}}))))

(println (= (set (let [ss "vwxyz"] (for [i ss, j ss, k ss, l ss] (str i j k l))))
   (set (gen-strings9 '{:states #{q0 q1 q2 q3 q4}
                        :alphabet #{v w x y z}
                        :start q0
                        :accepts #{q4}
                        :transitions {q0 {v q1, w q1, x q1, y q1, z q1}
                                      q1 {v q2, w q2, x q2, y q2, z q2}
                                      q2 {v q3, w q3, x q3, y q3, z q3}
                                      q3 {v q4, w q4, x q4, y q4, z q4}}}))))

(println (let [res (take 2000 (gen-strings9 '{:states #{q0 q1}
                                              :alphabet #{0 1}
                                              :start q0
                                              :accepts #{q0}
                                              :transitions {q0 {0 q0, 1 q1}
                                                            q1 {0 q1, 1 q0}}}))]
                  (and (every? (partial re-matches #"0*(?:10*10*)*") res)
                       (= res (distinct res)))))

(println (let [res (take 2000 (gen-strings9 '{:states #{q0 q1}
                                              :alphabet #{n m}
                                              :start q0
                                              :accepts #{q1}
                                              :transitions {q0 {n q0, m q1}}}))]
                  (and (every? (partial re-matches #"n*m") res)
                       (= res (distinct res)))))

(println (let [res (take 2000 (gen-strings9 '{:states #{q0 q1 q2 q3 q4 q5 q6 q7 q8 q9}
                                              :alphabet #{i l o m p t}
                                              :start q0
                                              :accepts #{q5 q8}
                                              :transitions {q0 {l q1}
                                                            q1 {i q2, o q6}
                                                            q2 {m q3}
                                                            q3 {i q4}
                                                            q4 {t q5}
                                                            q6 {o q7}
                                                            q7 {p q8}
                                                            q8 {l q9}
                                                            q9 {o q6}}}))]
                  (and (every? (partial re-matches #"limit|(?:loop)+") res)
                       (= res (distinct res)))))