
;; Palindromic Numbers

;; A palindromic number is a number that is the same when written forwards or backwards (e.g., 3, 99, 14341).
;; Write a function which takes an integer n, as its only argument, and returns an increasing 
;; lazy sequence of all palindromic numbers that are not less than n.

(defn make-palindromes [n]
  (let [; converts a number like `192` into vector of digits `[1 9 2]`
        vectorize (fn [n]
                    (let [m {\0 0, \1 1, \2 2, \3 3, \4 4, \5 5, \6 6, \7 7, \8 8, \9 9}]
                      (loop [v (str n), result []]
                        (if (empty? v)
                          result
                          (recur 
                            (rest v)
                            (conj result (m (first v))))
                          ))))
        ; converts a vector of digits `[1 9 1]` to number `191`
        numerize (fn [v] (Long/parseLong (apply str v)))
        ; for input palindrome creates the next palindrome
        mnp (fn [n]
              (let [
                    ; vector of digits for the input number `n`
                    v (vectorize n)
                    ; number of digits
                    c (count v)
                    ; numberal divistion (the middile) 5/2 = 2 (reminder 1) 
                    x (quot c 2)
                    ; create maximal palindrome for number of digits specified by `n`
                    cmp (fn [] (repeat c 9))
                    ; create next palindrome for number of input digits `n`
                    cnfp (fn [n] (assoc (vec (repeat n 0)) 0 1 (dec n) 1))
                    ; make a palindrome from a palindrome
                    mpfp (fn [] 
                           (loop [i (if (even? c) (dec x) x)
                                  j x 
                                  result v]
                             (if (or (nil? i) (neg? i))
                               result 
                               (let [a (result i) 
                                     b (result j)]
                                 (if (= i j)
                                   (if (not= 9 a)
                                     (recur nil      nil      (assoc result i (inc a)))   
                                     (recur (dec i) (inc j)   (assoc result i 0)))
                                   (if (not= 9 a)
                                      (recur nil     nil      (assoc result i (inc a) j (inc b)))
                                      (recur (dec i) (inc j)  (assoc result i 0 j 0))))
                                 ))))       
                    ]
                (if (= v (cmp))
                  (numerize (-> c inc cnfp))
                  (numerize (mpfp))
                  )))
        palindrome? (fn [n]
                      (let [s (str n)]
                        (= s (apply str (reverse s)))))
        ; make a palindrome from not a palindrome
        mpfnotp (fn [n]
                  (let [v (vectorize n)
                        c (count v)
                        x (quot c 2)
                        ; increment numbers inside out 12931 => 13931 
                        incsv (fn [vv, ii, jj, a, b]
                                (loop [vv1 vv
                                       o (if (odd? c) x (dec x))  
                                       p x]
                                  (if (< o ii)
                                    vv1
                                    (let [elo (vv1 o)
                                          elp (vv1 p)
                                          no (dec o)
                                          np (inc p)]
                                      (cond
                                        (and (= o p)    (< a b)   (not= 9 elo)) (recur (assoc vv1 jj a o (inc elo)) no np)
                                        (and (= o p)    (< a b)   (= 9 elo))    (recur vv1 no np)
                                        (and (= o p)    (= a b))                (recur vv1 no np)
                                        (and (not= o p) (= 9 elo) (= 9 elp))    (recur vv1 no np)
                                        (and (not= o p) (< elo elp))            (recur (assoc vv1 o elp) no np)
                                        (and (not= o p) (> elo elp))            (recur (assoc vv1 p elo) no np)
                                        (and (not= o p) (= elo elp))            (recur vv1 no np))
                                      ))))]
                    (loop [i 0
                           j (dec c) 
                           result v]
                      (if (or 
                            (and (even? c) (> i x))
                            (and (odd? c) (= i x)))
                        result
                        (let [a (result i)
                              b (result j)
                              ni (inc i)
                              nj (dec j)]
                          (cond 
                            (= a b) (recur ni nj result)
                            (< a b) (recur ni nj (incsv result i j a b))
                            (> a b) (recur ni nj (incsv (assoc result j a) i j a a)))
                          )))))
        first-pal (if (palindrome? n) n (-> n mpfnotp numerize))
        ]
    (
     (fn mps [n]
       (cons 
         n
         (lazy-seq
           (mps (mnp n)))))
     first-pal)
    ))

;; Tests

(println (= (take 26 (make-palindromes 0))
            [0 1 2 3 4 5 6 7 8 9 
             11 22 33 44 55 66 77 88 99 
             101 111 121 131 141 151 161]))

(println (= (take 16 (make-palindromes 162))
            [171 181 191 202 
             212 222 232 242 
             252 262 272 282 
             292 303 313 323]))

(println (= (take 6 (make-palindromes 1234550000))
            [1234554321 1234664321 1234774321 
             1234884321 1234994321 1235005321]))

(println (= (first (make-palindromes (* 111111111 111111111)))
            (* 111111111 111111111)))

(println (= (set (take 199 (make-palindromes 0)))
            (set (map #(first (make-palindromes %)) (range 0 10000)))))

(println (= true (apply < (take 6666 (make-palindromes 9999999)))))

(println (= (nth (make-palindromes 0) 10101) 9102019))