(defn my-reduce
  ([f, ll] (my-reduce f 0 ll))
  ([f, acc, ll] (my-reduce f acc (first ll) (rest ll)))
  ([f, acc, head, tail]
   (if (= head nil)
     acc
     (my-reduce f (f acc head) tail))))

(defn my-map [f ll]
  (my-reduce (fn [res item]
               (conj res (f item)))
             [] ll))

(defn my-filter [f ll]
  (my-reduce (fn [res item]
               (if (f item)
                 (cons item res)
                 res))
             [] ll))

(defn removeDuplicates [char tmp]
  (my-filter
   (fn [tmpEl] (not= (first tmpEl) char))
   tmp))

(defn constructWithChar [char tmp]
  (my-map
   (fn [alpha] (cons char alpha))
   (removeDuplicates char tmp)))

(defn generateSeq [alphabet tmp]
  (my-reduce concat
             '()
             (my-map
              (fn [char]  (reverse (constructWithChar char tmp)))
              alphabet)))

(defn lab14
  ([alphabet n] (lab14 alphabet (my-map (fn [letter] (list letter)) alphabet) (dec n)))
  ([alphabet tmp n]
   (if (= n 0)
     tmp
     (recur alphabet (generateSeq alphabet tmp) (dec n)))))

(lab14 '("a" (:b 1) ['c 'd]) 3)
