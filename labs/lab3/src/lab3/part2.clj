(ns lab3.part2)

;; Lazy version of partition function
(defn my-partition-all
  "Lazy partition seq into chuncks of length = len"
  ([len coll]
   (lazy-seq
    (when-let [ss (seq coll)]
      (let [chunk (doall (take len ss))]
        (cons
         chunk
         (my-partition-all len (drop len ss))))))))

(defn simulate-heavy-2
  "Simulate heavy test condition"
  ([el]
   (Thread/sleep 100)
   (even? el)))

(defn my-pmap
  "Parallel map function"
  ([f coll]
   (let [rets (map #(future (f %)) coll)
         step (fn step [[head & leftover]]
                (lazy-seq
                 (if-let [s (seq leftover)]
                   (cons (deref head) (step s))
                   (deref head))))]
     (step rets))))

(defn lazy-parallel-filter
  "Lazy parallel filter"
  [pred coll block-size]
  (->>
   coll
   (my-partition-all block-size)
   (my-pmap #(doall (filter pred %)))
   (apply concat)))

;; (time (take 10 (lazy-parallel-filter simulate-heavy-2 (range) 2)))