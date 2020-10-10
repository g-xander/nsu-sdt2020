(defn appendLetter
  ([letter tmp] (appendLetter letter tmp '()))
  ([letter tmp acc]
   (if (= (count tmp) 0)
     acc
     (if (= (first (first tmp)) letter)
       (recur letter (rest tmp) acc)
       (recur letter (rest tmp) (cons (cons letter (first tmp)) acc))))))

(defn constructSeq
  ([alphabet tmp] (constructSeq alphabet tmp '()))
  ([alphabet tmp acc]
   (if (= (count alphabet) 0)
     acc
     (recur (rest alphabet) tmp (concat acc (reverse (appendLetter (first alphabet) tmp)))))))

(defn lab12
  ([alphabet n] (lab12 alphabet n '(())))
  ([alphabet n res]
   (if (= n 0)
     res
     (recur alphabet (dec n) (constructSeq alphabet res)))))

(lab12 '("a" (:b 1) ['c 'd]) 3)
