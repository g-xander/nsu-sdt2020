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
  "@n-threads - how many chunks to run in parallel
   @pred - condition
   @coll - collection
   returns lazy-parallel-filtered collection"

  [n-threads pred coll]
  (when-let [ss (seq coll)]
    (let [batch (take n-threads coll)]
      (flatten (concat
                (doall (map deref (doall
                                   (map #(future (doall (filter pred %))) batch))))
                ;; First batch will always be calculated
                ;; Also with this: (time ) prints correct value of execution time
                ;; compared to wrapping whole run-batch-parallel body func in lazy-seq
                (lazy-seq (run-batch-parallel n-threads pred (drop n-threads ss))))))))

(defn lazy-parallel-filter
  "@pred - condition
   @coll - collection
   @chunk-size - how many elements in one split of @coll
   @batch-size - how many chunks to run in parallel
   @n - number of elements to return
   returns lazy-parallel-filtered collection"

  [pred coll chunk-size batch-psize n]
  (->>
   coll
   (my-partition-all chunk-size)
   (run-batch-parallel batch-psize pred)
   (take n)))

;; (time (lazy-parallel-filter simulate-heavy-2 (range) 2 10 10))