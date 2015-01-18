
;; Anagram Finder

;; Write a function which finds all the anagrams in a vector of words. 
;; A word x is an anagram of word y if all the letters in x can be rearranged in a different order to form y. 
;; Your function should return a set of sets, where each sub-set is a group of words which are anagrams of each other. 
;; Each sub-set should have at least two words. Words without any anagrams should not be included in the result.

(defn anagram-finder [words]
  (->> 
    (range)                                           ;; 0 1 2 3 4 5 6 7 8 9 10 ...
    (take (count words))                              ;; 0 1 2 3 4 5
    (map vector (map #(apply str (sort %)) words))    ;; [aemt 0] [amt 1] [aemt 2] [aemt 3] [aet 4]
    (sort-by #(first %))                              ;; [aemt 0] [aemt 2] [aemt 3] [aet 4] [amt 1]
    (partition-by #(first %))                         ;; ([aemt 0] [aemt 2] [aemt 3]) ([aet 4]) ([amt 1])
    (filter #(> (count %) 1))                         ;; ([aemt 0] [aemt 2] [aemt 3])
    (map (fn [s] (map #(second %) s)))                ;; (0 2 3)
    (map (fn [s] (set (map #(words %) s))))           ;; (#{meat team mate})
    (set)                                             ;; #{#{meat team mate}}
  ))

;; Tests

(println (= (anagram-finder ["meat" "mat" "team" "mate" "eat"])
   #{#{"meat" "team" "mate"}}))

(println (= (anagram-finder ["veer" "lake" "item" "kale" "mite" "ever"])
   #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}}))