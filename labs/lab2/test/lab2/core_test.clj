(ns lab2.core-test
  (:require [clojure.test :refer :all]
            [lab2.core :refer :all]))

(defmacro calcTime
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (list (/ (double (- (. System (nanoTime)) start#)) 1000000.0) ret#)))

(deftest recursive-integrator-test
  (testing "Correctness of integral calulation"
    (is (= 2666 (int ((lab2.part0/make-recursive-integrator lab2.part0/square 1/10) 20))))
    (is (= 40000 (int ((lab2.part0/make-recursive-integrator lab2.part0/cube 1/10) 20))))))

(deftest memoized-integrator-test
  (testing "Correctness of memoized-integral calulation"
    (is (= 2666 (int ((lab2.part1/make-memoized-integrator lab2.part0/square 1/10) 20))))
    (is (= 40000 (int ((lab2.part1/make-memoized-integrator lab2.part0/cube 1/10) 20))))))

(deftest memoized-integrator-test
  (testing "Correctness of lazy-integral calulation"
    (is (= 2666 (int (first (nth (lab2.part2/make-lazy-integrator lab2.part0/square 1/10) 199)))))
    (is (= 40001 (int (first (nth (lab2.part2/make-lazy-integrator lab2.part0/cube 1/10) 199)))))))

(deftest memoize-test
  (testing "Caching abilities of memoized integral"
    (let [itg (lab2.part1/make-memoized-integrator lab2.part0/square 1/10)
          t1 (first (calcTime (itg 20)))
          t2 (first (calcTime (itg 30)))
          t3 (first (calcTime (itg 27)))]

      (is (> t1 t2))
      (is (> t2 t3)))))

(deftest lazy-seq-test
  (testing "Caching abilities of lazy-seq integral"
    (let [itg (lab2.part2/make-lazy-integrator lab2.part0/square 1/10)
          t1 (first (calcTime (nth itg 200)))
          t2 (first (calcTime (nth itg 300)))
          t3 (first (calcTime (nth itg 270)))]

      (is (> t1 t2))
      (is (> t2 t3)))))