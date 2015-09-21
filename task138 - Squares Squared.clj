
;; Squares Squared

;; Create a function of two integer arguments: the start and end, respectively. 
;; You must create a vector of strings which renders a 45° rotated square of integers 
;; which are successive squares from the start point up to and including the end point. 
;; If a number comprises multiple digits, wrap them around the shape individually. 
;; If there are not enough digits to complete the shape, fill in the rest with asterisk characters. 
;; The direction of the drawing should be clockwise, starting from the center of the shape and working outwards, 
;; with the initial direction being down and to the right.

;; Squeares Of Squares
(defn sos [a b]
  (let [
        ; calculate numbers for the square from a to b
        ; @param a - start
        ; @param b - end (including)
        calc-nums (fn [a b]
                    (loop [x a, result []]
                      (cond
                        (> x b) result
                        (= x b) (conj result x)
                        :else (recur (int (Math/pow x 2)) (conj result x)))))
        ; make input nums into sequence of digits
        ; @param nums - sequence of numbers
        make-dig-seq  (fn [nums]
                        (loop [n nums, result []]
                          (if-not (empty? n)
                            (recur (rest n) (conj result (map str (-> (first n) str seq))))
                            (flatten result))))
        ; make lazy collection of squares
        make-squares  (fn [] (map #(int (Math/pow % 2)) (iterate inc 1)))
        ; find the smalles square size into which fill all digits
        ; @param digits-count - count of digits for filling a square
        ; @param squares      - lazy sequence of squares 
        find-square-count (fn [digits-count, squares]
                            (if (= 1 digits-count)
                              1
                              (loop [sqs squares]
                                (if (<= digits-count (first sqs))
                                  (first sqs)
                                  (recur (rest sqs))))))
        ; make a sequence of digits and asterisk to make a square
        ; @param digits  - sequence of digits which will be filled with asterisks
        ; @param squares - lazy sequence of squares 
        fill-with-asterisks (fn  [digits squares]
                              (let [digits-count (count digits)
                                    square-count (find-square-count digits-count squares)
                                    asts-count (- square-count digits-count)]
                                (if (zero? asts-count)
                                  digits
                                  (concat digits (take asts-count (repeatedly (fn [] "*")))))))
        ; calculate the array into which chars will be rendered
        ; for example for 9 chars we need 5x5 array, so the edge is 5
        ; @param chars - digits and characters, which count is a square
        calc-array-edge (fn [chars]
                          (let [sqrt-val (int (Math/sqrt (count chars)))]
                            (+ sqrt-val (dec sqrt-val))))
        ; calculate coordinates of the starting point
        ; @param chars-count  - count of charcters which will be rendered
        ; @param array-length - lenght of an array edge 
        calc-start  (fn  [chars-count, array-length]
                      (let [arr-center (if (even? array-length) (/ array-length 2) (/ (dec array-length) 2))]
                        (if (odd? chars-count)
                          [arr-center arr-center]
                          [(dec arr-center) arr-center])))
        ; make lazy collection of directions for rendering squares 
        make-directions (fn []
                    (let [directions [:DR :DL :UL :UR]
                          count-dirs (count directions)
                          next-dir  (fn [n]
                                      (if (< n count-dirs)
                                        (directions n)
                                        (directions (rem n count-dirs))))]
                      (map #(next-dir %) (iterate inc 0))))
        ; generate an array and fill it with spaces - " "
        ; @param length - arrays edge length
        gen-array (fn [length]
                    (let [idxs    (take length (iterate inc 0))
                          gen-row (fn [] (vec (for [x idxs] " ")))]
                      (vec (for [y idxs] (gen-row)))))
        ; render one char designated coordinates in an array
        ; @param array - array into which a char will be rdered
        ; @param cords - coordinates at which a char will be rendered
        ; @param c     - char to render
        render-char (fn [array cords c]
                      (let [x (cords 0)
                            y (cords 1)
                            row (array x)
                            upd-row (assoc row y c)]
                        (assoc array x upd-row)))
        ; calculate new cordinates based on previous coordinates and a direction
        ; prev-c-cords - previous coordinates
        ; dir          - direction (down-right, down-left, up-left, up-right)
        calc-new-cord (fn [prev-c-cords dir]
                        (let [directions {:DR [1 1], :DL [1 -1], :UL [-1 -1], :UR [-1 1]}
                              ctrls (directions dir)
                              nx (+ (prev-c-cords 0) (ctrls 0))
                              ny (+ (prev-c-cords 1) (ctrls 1))]
                          [nx ny]))
        ; render one char at direction from previous coordinate
        ; @param array        - array into which a char will be rendered
        ; @param prev-c-cords - previous coordinates from which to make a direction
        ; @param dir          - type of direction
        ; @param c            - char to render 
        render-char-at-dir  (fn [array prev-c-cords dir c] 
                              (render-char array (calc-new-cord prev-c-cords dir) c))
        ; render a line of characters
        ; @param array - array into which characters will be rendered
        ; @param start - starting point for a line (excluding)
        ; @param dir   - direction of a line
        ; @param chars - characters rendered in a line
        ; @return a map with an array and last rendered coordinates
        render-line (fn [array start dir chars]
                      (loop [a array prev-c-cords start cs chars]
                        (if (empty? cs)
                          {:array a, :prev-cord prev-c-cords}
                          (recur
                            (render-char-at-dir a prev-c-cords dir (first cs))
                            (calc-new-cord prev-c-cords dir)
                            (rest cs)))))
        ; split chars into groups which will be then rendered as lines at specific directions
        ; example: [1 2 3 4 5 6 7 8 9] - [2] [3] [4 5] [6 7] [8 9]
        ; the previous point is not considered as part of a line (therefore the first char is ignored)
        ; @param chars - characters to be split
        split-to-lines  (fn [chars]
                          (loop [cs (rest chars) counter 1 used 0 result []]
                            (if (empty? cs)
                              result
                              (let [splited (split-at counter cs)]
                                (recur
                                  (splited 1)
                                  (if (= 1 used) (inc counter) counter)
                                  (if (= 1 used) 0 (inc used))
                                  (conj result (splited 0)))))))
        ;;;;
        square-chars (fill-with-asterisks (make-dig-seq (calc-nums a b)) (make-squares))
        array-length (calc-array-edge square-chars)
        array        (gen-array array-length)
        start        (calc-start (count square-chars) array-length)
        array-with-s (render-char array start (first square-chars))
        lines        (split-to-lines square-chars)]
  (loop [l lines a array-with-s d (make-directions) s start]
    (if (empty? l) 
      (map #(apply str %) a)
      (let [{arr :array, prev-cord :prev-cord} (render-line a s (first d) (first l))]
        (recur
          (rest l)
          arr
          (rest d)
          prev-cord))))))

;; Tests

(println (= (sos 2 2) ["2"]))

(println (= (sos 2 4) [" 2 "
                       "* 4"
                       " * "]))

(println (= (sos 3 81) [" 3 "
                        "1 9"
                        " 8 "]))

(println (= (sos 4 20) [" 4 "
                        "* 1"
                        " 6 "]))

(println (= (sos 2 256) ["  6  "
                         " 5 * "
                         "2 2 *"
                         " 6 4 "
                         "  1  "]))

(println (= (sos 10 10000) ["   0   "
                            "  1 0  "
                            " 0 1 0 "
                            "* 0 0 0"
                            " * 1 * "
                            "  * *  "
                            "   *   "]))