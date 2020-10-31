(ns lab2.part2)

(defn square [n] (* n n))

(defn do-trapezoidal-rule-step-2
  "Caculate bounds for n-step and apply trapezoidal rule to them @f - function step @n - step counter dx - step size"
  ([f stop dx]

   (let [b stop
         a (- stop dx)]
    ;  (println "[DEBUG] Calling with a=" a " and b=" b)
     (* (- b a) (/ (+ (f a) (f b)) 2)))))

(defn make-lazy-integrator
  "Lazy sequence version. Find the definite integral of single argument function f @f - function @dx - step"
  ([f dx]
   (let [itg (fn itg
               ([] (iterate #(list
                              (+ (first %) (do-trapezoidal-rule-step-2 f (+ (last %) dx) dx))
                              (+ (last %) dx))
                            (list (do-trapezoidal-rule-step-2 f dx dx) dx))))
         wrap (itg)]
     (fn ([stop]
          (let [steps (quot stop dx)
                leftover (- stop (* steps dx))]
            (+
             (if (> steps 0)
               (first (nth wrap (- steps 1)))
               0)
             (if (= leftover 0)
               0
               (do-trapezoidal-rule-step-2 f stop leftover)))))))))

; (let [litg (make-lazy-integrator square 1/10)]
;   (println "1")
;   (time (float (litg 20)))
;   (println "2")
;   (time (float (litg 20)))
;   (println "3")
;   (time (float (litg 30)))
;   (println "4")
;   (time (float (litg 28)))
;   (println "5")
;   (time (float (litg 10))))