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
   (Thread/sleep 1000)
   (even? el)))

(defn run-batch-parallel
  "@pred - condition
   @coll - collection
   returns lazy-parallel-filtered collection"

  [pred coll]
  (lazy-seq
   (when-let [ss (seq coll)]
     (doall (map deref (doall (map #(future (doall (filter pred %))) ss)))))))

(defn lazy-parallel-filter
  "@pred - condition
   @coll - collection
   @chunk-size - how many elements in one split of @coll
   @batch-size - how many chunks to run in parallel
   @n - number of elements to return
   returns lazy-parallel-filtered collection"

  [pred coll chunk-size batch-psize]
  (->>
   coll
   (my-partition-all chunk-size)
   (my-partition-all batch-psize)
   (map (partial run-batch-parallel pred))
   (mapcat identity) ; remove non-existing elements
   (apply concat)))

;; (time (take 10 (lazy-parallel-filter simulate-heavy-2 (range) 2 5)))
