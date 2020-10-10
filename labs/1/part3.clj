(defn my-reduce
  ([f, ll] (my-reduce f 0 ll))
  ([f, acc, ll] (my-reduce f acc (first ll) (rest ll)))
  ([f, acc, head, tail]
   (if (= head nil)
     acc
     (my-reduce f (f acc head) tail))))

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
                 (conj res item)
                 res))
             [] ll))

(my-map inc [1 2 3 4 5])
(my-filter even? [1 2 3 4 5])