(defn r-my-reduce
  ([f, ll] (r-my-reduce f 0 ll))
  ([f, acc, ll]
   (if (= (count ll) 0)
     acc
     (recur f (f acc (first ll)) (rest ll)))))

(defn my-map [f ll]
  (r-my-reduce (fn [res item]
                 (conj res (f item)))
               [] ll))

(defn my-filter [f ll]
  (r-my-reduce (fn [res item]
                 (if (f item)
                   (cons item res)
                   res))
               [] ll))

(defn append-letter ([alphabet tmp]
                     (r-my-reduce
                      concat '()
                      (my-map
                       (fn [tmpEl]
                         (reverse
                          (my-map
                           (fn [letter] (cons letter tmpEl))
                           (my-filter (fn [alphaLetter] (not= (first tmpEl) alphaLetter)) alphabet))))
                       tmp))))

(defn lab14
  ; ([alphabet n] (lab14 alphabet '(()) n))
  ([alphabet n]
   (my-map (fn [ss] (reverse ss)) (nth (iterate (fn [tmp] (append-letter alphabet tmp)) (my-map list alphabet)) (dec n)))))

(lab14 '("a" (:b 1) ['c 'd]) 3)
