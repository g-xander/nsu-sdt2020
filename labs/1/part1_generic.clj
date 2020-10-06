(defn lab11Generic
  ([alphabet, n] (lab11Generic alphabet 0 [] n -1))
  ([alphabet, iter, tmp, n, prevIter]
   (if (= n 0)
     (list tmp)
     (when (< iter (count alphabet))
       (concat []
               (when-not (= iter prevIter)
                 (lab11Generic alphabet 0 (conj tmp (nth alphabet iter)) (dec n) iter))
               (lab11Generic alphabet (inc iter) tmp n prevIter))))))

(lab11Generic '("a" (:b 1) ['c 'd]) 3)