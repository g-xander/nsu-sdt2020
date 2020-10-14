(ns lab2.part1)

(defn square [n] (* n n))

(defn do-trapezoidal-rule-step-1
  "Caculate bounds for n-step and apply trapezoidal rule to them @f - function step @n - step counter dx - step size"
  ([f stop dx]
   (if (<= stop 0)
     0
     (let [b stop
           a (- stop dx)]
      ;;  (println "[DEBUG] Calling with a=" a " and b=" b)
       (* (- b a) (/ (+ (f a) (f b)) 2))))))

(defn make-memoized-integrator
  "Memoized version. Find the definite integral of single argument function f @f - function @dx - step"
  ([f dx]
   (let [itg (memoize
              (fn [itg stop]
                (if (<= stop 0)
                  0
                  (+
                   (do-trapezoidal-rule-step-1 f stop dx)
                   (itg itg (- stop dx))))))
         r-itg (partial itg itg)]

     (fn [x] (let [whole (* (quot x dx) dx)
                   leftover (- x whole)]
               (+ (r-itg whole) (do-trapezoidal-rule-step-1 f x leftover)))))))

;; (let [mitg (make-memoized-integrator square 1/10)]
;;   (println "1")
;;   (time (float (mitg 20)))
;;   (println "2")
;;   (time (float (mitg 20)))
;;   (println "3")
;;   (time (float (mitg 30)))
;;   (println "4")
;;   (time (float (mitg 28)))
;;   (println "5")
;;   (time (float (mitg 10))))
