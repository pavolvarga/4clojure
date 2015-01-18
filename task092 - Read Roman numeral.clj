
;; Roman numerals are easy to recognize, but not everyone knows all the rules necessary to work with them. 
;; Write a function to parse a Roman-numeral string and return the number it represents. 

;; You can assume that the input will be well-formed, in upper-case, and follow the subtractive principle. 
;; You don't need to handle any numbers greater than MMMCMXCIX (3999), the largest number representable with ordinary letters.

(defn rn-reader [n]
	(let [tran-map {"I"  1
				    "V"  5
				    "X"  10
				    "L"  50
				    "C"  100
				    "D"  500
				    "M"  1000
				    "IV" 4
				    "IX" 9
				    "XL" 40
				    "XC" 90
				    "CD" 400
				    "CM" 900 }
		len 	(count n)
		digits (vec (for [x n] (str x)))	  
		crd	   (fn [l r] (apply str (concat l r)))
		is-val? (fn [v] (contains? tran-map v))]
	(loop [idx 0, result 0]
		(if (>= idx len)
			result
			(if (= idx (dec len))
				(recur (inc idx) (+ result (tran-map (digits idx))))
				(let [l (digits idx)
				  r (digits (inc idx))
				  valid (is-val? (crd l r))]
				  (if valid
				  	(recur (+ 2 idx) (+ result (tran-map (crd l r))))
				  	(recur (inc idx) (+ result (tran-map l)))))
			))
		)))

;; Tests

(println (= 14 (rn-reader "XIV")))

(println (= 827 (rn-reader "DCCCXXVII")))

(println (= 3999 (rn-reader "MMMCMXCIX")))

(println (= 48 (rn-reader "XLVIII")))