(ns lab3.core-test
  (:require [clojure.test :refer :all]
            [lab3.core :refer :all]))

(defmacro calcTime
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (list (/ (double (- (. System (nanoTime)) start#)) 1000000.0) ret#)))

;; Lab 3.1 tests
(deftest my-partition-test 
  (testing "Correctness of spliting seq")
  (is (= '((1 2) (3 4)) (lab3.part1/my-partition 2 '(1 2 3 4) )))
  (is (= '((1 2) (3 4) (5)) (lab3.part1/my-partition 2 '(1 2 3 4 5)))))

(deftest my-single-filter-test
  (testing "Correctness of single filter function")
  (is (= '(0 2 4 6 8 10 12 14 16 18) (lab3.part1/single-filter lab3.part1/simulate-heavy-1 (range 20) 4))))

(deftest my-parallel-filter-test
  (testing "Correctness of parallel filter function")
  (is (= '(0 2 4 6 8 10 12 14 16 18) (lab3.part1/parallel-filter lab3.part1/simulate-heavy-1 (range 20) 4))))

(deftest time-compare-single-parallel
  (testing "Testing performance single vs multi-threaded filter"
    (let [timeSf (first (calcTime (lab3.part1/single-filter lab3.part1/simulate-heavy-1 (range 20) 4)))
          timePf (first (calcTime (lab3.part1/parallel-filter lab3.part1/simulate-heavy-1 (range 20) 4)))]
      (is (> timeSf timePf)))))

;; Lab 3.2 tests
(deftest my-partition-all-test
  (testing "Correctness of spliting seq")
  (is (= '((0 1) (2 3)) (take 2 (lab3.part2/my-partition-all 2 (range)))))
  (is (= '((0 1) (2 3) (4)) (take 5 (lab3.part2/my-partition-all 2 (range 5))))))

(deftest my-lazy-parallel-filter-test
  (testing "Correctness of lazy-parallel filter function")
  (is (= '(0 2 4 6 8 10 12 14 16 18) (take 10 (lab3.part2/lazy-parallel-filter lab3.part2/simulate-heavy-2 (range) 2)))))

(deftest time-compare-single-lazyparallel
  (testing "Testing performance single vs multi-threaded filter"
    (let [timeSf (first (calcTime (lab3.part1/single-filter lab3.part1/simulate-heavy-1 (range 20) 4)))
          timeLPf (first (calcTime (take 10 (lab3.part2/lazy-parallel-filter lab3.part2/simulate-heavy-2 (range) 2))))]
      (is (> timeSf timeLPf)))))