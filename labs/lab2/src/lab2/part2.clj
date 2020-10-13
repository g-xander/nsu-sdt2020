(ns lab2.part2)

(defn square [n] (* n n))

(defn do-trapezoidal-rule-step-2
  "Caculate bounds for n-step and apply trapezoidal rule to them @f - function step @n - step counter dx - step size"
  ([f stop dx]
   (if (< stop 0)
     0
     (let [b stop
           a (- stop dx)]
      ;;  (println "[DEBUG] Calling with a=" a " and b=" b)
       (* (- b a) (/ (+ (f a) (f b)) 2))))))

(defn make-lazy-integrator
  "Lazy sequence version. Find the definite integral of single argument function f @f - function @dx - step"
  ([f dx]
   (let [itg
         (fn itg 
           ([] (itg (list (do-trapezoidal-rule-step-2 f dx dx) dx)))
           ([res]
           (lazy-seq (cons res (itg (list
                                     (+ (first res) (do-trapezoidal-rule-step-2 f (+ (last res) dx) dx))
                                     (+ (last res) dx)))))))]
     (itg))))

;; (let [litg (make-lazy-integrator square 1/10)]
;; 	(println "1")
;;   (time (first (nth litg 1000)))
;; 	(println "2")
;;   (time (first (nth litg 1005)))
;; 	(println "3")
;;   (time (first (nth litg 565)))
;; 	(println "4")
;;   (time (first (nth litg 1500)))
;; 	(println "5")
;;   (time (first (nth litg 1600))))