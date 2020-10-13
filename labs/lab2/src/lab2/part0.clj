(ns lab2.part0)

(defn square [n] (* n n))
(defn cube [n] (* n n n))

(defn do-trapezoidal-rule-step-0
  "Caculate bounds for n-step and apply trapezoidal rule to them @f - function step @n - step counter dx - step size"
  ([f stop dx]
   (if (< stop 0)
     0
     (let [b stop
           a (- stop dx)]
      ;;  (println "[DEBUG] Calling with a=" a " and b=" b)
       (* (- b a) (/ (+ (f a) (f b)) 2))))))

(defn make-recursive-integrator
  "Non-memoized version. Find the definite integral of single argument function f @f - function @dx - step"
  ([f dx]
   (let [itg (fn itg [stop]
                (if (< stop 0)
                  0
                  (+
                   (if (> stop dx)
                     (do-trapezoidal-rule-step-0 f stop dx)
                     (do-trapezoidal-rule-step-0 f stop (- dx stop)))
                   (itg (- stop dx)))))]
     (fn [x] (+ (itg x))))))

;; (let [itg (make-recursive-integrator square 1/10)]
;;   (println "1")
;;   (time (float (itg 20)))
;;   (println "2")
;;   (time (float (itg 20)))
;;   (println "3")
;;   (time (float (itg 30)))
;;   (println "4")
;;   (time (float (itg 28)))
;;   (println "5")
;;   (time (float (itg 10))))
