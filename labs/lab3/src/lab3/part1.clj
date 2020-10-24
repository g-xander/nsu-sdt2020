(ns lab3.part1)

;; non-lazy version of partition func
(defn my-partition
  "Partition seq into chuncks of length = len"
  ([len coll]
   (let [whole (* (quot (count coll) len) len)
         tail (mod (count coll) len)]
     (if (= tail 0)
       (my-partition len coll '())
       (concat
        (my-partition len (take whole coll) '())
        (list (drop whole coll))))))
  ([len coll acc]
   (if (= (count coll) 0)
     (reverse acc)
     (recur len (drop len coll) (cons (take len coll) acc)))))

(defn simulate-heavy-1
  "Simulate heavy test condition"
  ([el]
   (Thread/sleep 100)
   (even? el)))

(defn single-filter
  "Single-threaded version of filter"
  [pred coll block-size]
  (->>
   coll
   (partition block-size)
   (map #(filter pred %))
   (apply concat)
   (doall)))

(defn parallel-filter
  "Multi-threaded version of filter"
  [pred coll block-size]
  (->>
   coll
   (partition block-size)
   (map #(future (doall (filter pred %))))
   (doall)
   (map deref)
   (apply concat)))