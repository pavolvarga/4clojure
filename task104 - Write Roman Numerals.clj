
;; Write Roman Numerals

;; This is the inverse of Problem 92, but much easier. Given an integer smaller than 4000, 
;; return the corresponding roman numeral in uppercase, adhering to the subtractive principle.

(defn d-to-r [n]
  (let [conv-map 
        {0 {3 ""         , 2 ""    , 1 ""    , 0 ""    }
         1 {3 "M"        , 2 "C"   , 1 "X"   , 0 "I"   },
         2 {3 "MM"       , 2 "CC"  , 1 "XX"  , 0 "II"  },
         3 {3 "MMM"      , 2 "CCC" , 1 "XXX" , 0 "III" },
         4 {3 "MMMM"     , 2 "CD"  , 1 "XL"  , 0 "IV"  },
         5 {3 "MMMMM"    , 2 "D"   , 1 "L"   , 0 "V"   },
         6 {3 "MMMMMM"   , 2 "DV"  , 1 "LX"  , 0 "VI"  },
         7 {3 "MMMMMMM"  , 2 "DCC" , 1 "LXX" , 0 "VII" },
         8 {3 "MMMMMMMM" , 2 "DCCC", 1 "LXXX", 0 "VIII"},
         9 {3 "MMMMMMMMM", 2 "CM",   1 "XC"  , 0 "IX"  }}
        digits (map #(-> % str Integer/parseInt) (seq (str n)))
        pows   (reverse (take (count digits) (range))) 
        get-value (fn [digit pow] (get-in conv-map [digit pow]))]
    (loop [ds digits, ps pows, result []]
      (if (empty? ds)
        (apply str result)
        (recur 
          (rest ds) 
          (rest ps) 
          (conj result (get-value (first ds) (first ps)) ))
        ))
    ))

;; Tests

(println (= "I" (d-to-r 1)))

(println (= "XXX" (d-to-r 30)))

(println (= "IV" (d-to-r 4)))

(println (= "CXL" (d-to-r 140)))

(println (= "DCCCXXVII" (d-to-r 827)))

(println (= "MMMCMXCIX" (d-to-r 3999)))

(println (= "XLVIII" (d-to-r 48)))