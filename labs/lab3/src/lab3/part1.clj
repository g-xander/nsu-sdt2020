(ns lab3.part1)

(defn my-partition
([len coll]
 (when-let [ss (seq coll)]
   (let [chunk (take len ss)]
     (cons chunk (my-partition len (drop len ss)))))))

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
   (my-partition block-size)
   (map #(future (doall (filter pred %))))
   (doall)
   (mapcat deref)))